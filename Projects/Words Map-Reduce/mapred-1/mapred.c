/* mapred.c - Map-Reduce to count the occurrences of words in a file */
/* Frederic-Gerald Morcos (fredmorcos) <fred.morcos@gmail.com> */

#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <math.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <sysexits.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <hashmap.h>

/* Structure to store timing information. */
struct time_info {
  clock_t begin_cpu;
  clock_t end_cpu;

  struct timespec begin_rt;
  struct timespec end_rt;

  struct timeval begin_wc;
  struct timeval end_wc;

  long double cpu;
  long double wc;
  long double rt;
};

/* Timing function prototypes. */
void ti_now(struct time_info *const ti, const bool begin);
void ti_diff(struct time_info *const ti);

/* Store the current time into the begin section of a time_info
   structure. */
void ti_now(struct time_info *const ti, const bool begin) {
  assert(ti != NULL);

  if (begin) {
    ti->begin_cpu = clock();
    clock_gettime(CLOCK_REALTIME, &ti->begin_rt);
    gettimeofday(&ti->begin_wc, NULL);
  } else {
    ti->end_cpu = clock();
    clock_gettime(CLOCK_REALTIME, &ti->end_rt);
    gettimeofday(&ti->end_wc, NULL);
  }
}

/* Store the current time into the end section of a time_info
   structure, and store the time difference between the begin and end
   sections. */
void ti_diff(struct time_info *const ti) {
  assert(ti != NULL);

  static const unsigned int million = 1000000;
  static const unsigned int billion = 1000000000L;

  time_t sec, fsec;

  ti_now(ti, false);

  /* cpu time */
  ti->cpu = (ti->end_cpu - ti->begin_cpu) / (long double) CLOCKS_PER_SEC;
  assert(ti->cpu >= 0);

  /* real-time */
  if (ti->begin_rt.tv_nsec > ti->end_rt.tv_nsec) {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec - 1;
    fsec = ti->end_rt.tv_nsec + billion - ti->begin_rt.tv_nsec;
  } else {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec;
    fsec = ti->end_rt.tv_nsec - ti->begin_rt.tv_nsec;
  }

  ti->rt = sec + (long double) 1.e-9 * fsec;
  assert(ti->rt >= 0);

  /* wall-clock time */
  if (ti->begin_wc.tv_usec > ti->end_wc.tv_usec) {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec - 1;
    fsec = ti->end_wc.tv_usec + million - ti->begin_wc.tv_usec;
  } else {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec;
    fsec = ti->end_wc.tv_usec - ti->begin_wc.tv_usec;
  }

  ti->wc = sec + (long double) 1.e-6 * fsec;
  assert(ti->wc >= 0);
}

/* Declare and define hashmap functions from strings -> size_t. */
HASHMAP_FUNCS_DECLARE(word, char, size_t)
HASHMAP_FUNCS_CREATE(word, char, size_t)
void hashmap_free(struct hashmap *const m);

void hashmap_free(struct hashmap *const m) {
  for (struct hashmap_iter *iter = hashmap_iter(m);
       iter;
       iter = hashmap_iter_next(m, iter))
    {
      free(word_hashmap_iter_get_data(iter));
    }

  hashmap_destroy(m);
  free(m);
}

/* Convenience structure for keeping a word and its occurrences. */
struct word {
  const char *ptr;             /* Pointer to nul-terminated string. */
  size_t occ;                  /* Number of occurrences. */
};

struct params {
  char *const buf;
  const size_t size;
  const size_t depth;
};

/* Helpers for casting function pointers. */
typedef int (*cmpfunc)(const void *, const void *);
typedef void *(*tfunc)(void *);

/* Prototypes. */
int countcmp(const struct word *w1, const struct word *w2);
struct hashmap *count(char *buf);
struct hashmap *merge(struct hashmap *const m1,
                      const struct hashmap *const m2);
struct hashmap *mapred(const struct params *const params);

/* Compare two words according to their occurrences. */
int countcmp(const struct word *w1, const struct word *w2) {
  if (w1->occ == w2->occ) {
    return strcmp(w1->ptr, w2->ptr);
  } else if (w1->occ > w2->occ) {
    return -1;
  } else {
    return 1;
  }
}

/* Count the words occurrences in a buffer. */
struct hashmap *count(char *buf) {
  typedef unsigned char uchar;

  /* Allocate the hashmap. */
  struct hashmap *words = malloc(sizeof(struct hashmap));

  if (words == NULL) {
    return NULL;
  }

  /* Initialize the hashmap. */
  hashmap_init(words,
               hashmap_hash_string,
               hashmap_compare_string,
               10000);

  while (true) {
    /* Nullify any spaces and punctuation. */
    while (*buf != '\0' && !isalnum((uchar) *buf)) {
      *buf = '\0';
      buf++;
    }

    /* Stumbled onto the end of the buffer, so exit. */
    if (*buf == '\0') {
      break;
    }

    /* Found the start of a word. */
    const char *const w = buf;

    /* Consume the characters of the word and lowercase them. */
    while (isalnum((uchar) *buf)) {
      *buf = (char) tolower(*buf);
      buf++;
    }

    /* Reached a non-alphanumeric: If the current character is the end
       of the buffer, then the last word has been traversed and the
       buffer pointer must not be incremented, otherwise mark the end
       of the current word with a nul-termination and increment the
       pointer to move onto the next word at the next iteration. */
    if (*buf != '\0') {
      *buf = '\0';
      buf++;
    }

    /* Allocate the occurrence for this word, so far = 1. */
    size_t *occ = malloc(sizeof(size_t));

    if (occ == NULL) {
      return NULL;
    }

    *occ = 1;

    /* Will return `occ` if the key did not already exist in the
       hashmap, otherwise will return a pointer for the already
       existing values corresponding to the key. */
    size_t *res = word_hashmap_put(words, w, occ);

    if (res == NULL) {
      free(occ);
      return NULL;
    }

    /* The word already existed in the hashmap. */
    if (res != occ) {
      /* Free the new occurrence = 1 we created. */
      free(occ);
      /* Increment the already existing occurrence value. */
      *res += 1;
    }
  }

  return words;
}

/* Merge the second hashmap `m2` into into the first one `m1`. */
struct hashmap *merge(struct hashmap *const m1, const struct hashmap *const m2) {
  /* Iterate over the words in the second hashmap, and insert each of
     them into the first one if it doesn't already exist. If it
     already exist in the first hashmap, then increment its occurrence
     with the occurrence from the second hashmap. */
  for (struct hashmap_iter *iter = hashmap_iter(m2);
       iter;
       iter = hashmap_iter_next(m2, iter))
    {
      const char *word = word_hashmap_iter_get_key(iter);
      size_t *const occ = word_hashmap_iter_get_data(iter);

      /* Allocate the occurrence for this word. */
      size_t *const new_occ = malloc(sizeof(size_t));

      if (new_occ == NULL) {
        return NULL;
      }

      *new_occ = *occ;

      /* Will return `new_occ` if the key did not already exist in the
         hashmap, otherwise will return a pointer for the already
         existing values corresponding to the key. */
      size_t *res = word_hashmap_put(m1, word, new_occ);

      if (res == NULL) {
        free(new_occ);
        return NULL;
      }

      /* The word already existed in the first hashmap. */
      if (res != new_occ) {
        /* Free the new occurrence we created. */
        *res += *new_occ;
        /* Increment the already existing occurrence value with the
           occurrence value from the second hashmap. */
        free(new_occ);
      }
    }

  return m1;
}

/* Main helper prototypes. */
size_t parse_nthreads(const char *const str);
size_t filesize(const char *const fn);
char *readfile(FILE *const fp, size_t size, size_t *const total);

/* Parse the number of threads from a string. */
size_t parse_nthreads(const char *const str) {
  const char *const str_end = str + strlen(str);
  char *endptr = NULL;

  errno = 0;
  const long n_tmp = strtol(str, &endptr, 10);

  if (errno != 0 || endptr == str || endptr != str_end) {
    err(EX_USAGE, "cannot parse number of threads");
  }

  if (n_tmp <= 0) {
    errx(EX_USAGE, "invalid number of threads");
  }

  return (size_t) n_tmp;
}

/* Get the size of a file. */
size_t filesize(const char *const fn) {
  struct stat st;

  if (stat(fn, &st) == -1) {
    err(EX_OSFILE, "cannot stat %s", fn);
  }

  if (st.st_size < 0) {
    errx(EX_OSFILE, "invalid filesize %ld", st.st_size);
  }

  if (st.st_size == 0) {
    errx(EX_OSFILE, "file has zero length");
  }

  return (size_t) st.st_size;
}

char *readfile(FILE *const fp, size_t size, size_t *const total) {
  /* Read the entire file at once, for really huge files `mmap()`
     would probably be better. */
  char *const buf = malloc(size + 1);

  if (buf == NULL) {
    warn("cannot allocate space to load file");
    return NULL;
  }

  *total = 0;

  while (true) {
    const size_t read = fread(buf + *total, 1, size - *total, fp);

    *total += read;

    assert(*total <= size);

    if (*total < size) {
      if (ferror(fp) != 0) {
        warn("cannot read file");
        free(buf);
        return NULL;
      }

      if (feof(fp) != 0) {
        break;
      }

      continue;
    }

    break;
  }

  buf[size] = '\0';

  return buf;
}

struct hashmap *mapred(const struct params *const params) {
  if (params->depth == 0) {
    return count(params->buf);
  }

  size_t chunksize = params->size / 2;

  /* If the end of the chunk lands in the middle of the word,
     include that word in the chunk. */
  while (isalnum(params->buf[chunksize])) {
    chunksize++;
  }

  /* Mark the end of the word. */
  params->buf[chunksize] = '\0';

  pthread_t t1;

  struct params t1_params =
    { .buf = params->buf,
      .size = chunksize,
      .depth = params->depth - 1
    };

  if (pthread_create(&t1, NULL, (tfunc) mapred, &t1_params) != 0) {
    warnx("cannot create thread 1");
    return NULL;
  }

  void *t2_ret = NULL;
  bool t2_ret_valid = false;

  if (chunksize != params->size) {
    pthread_t t2;

    struct params t2_params =
      { .buf = params->buf + chunksize,
        .size = params->size - chunksize,
        .depth = params->depth - 1
      };

    if (pthread_create(&t2, NULL, (tfunc) mapred, &t2_params) != 0) {
      warnx("cannot create thread 2");

      if (pthread_kill(t1, SIGKILL) != 0) {
        warnx("cannot kill thread 1");
      }

      return NULL;
    }

    if (pthread_join(t2, &t2_ret) != 0) {
      warn("cannot join thread 2");

      if (pthread_kill(t1, SIGKILL) != 0) {
        warnx("cannot kill thread 1");
      }

      return NULL;
    }

    if (t2_ret == NULL) {
      warnx("thread 2 returned null");

      if (pthread_kill(t1, SIGKILL) != 0) {
        warnx("cannot kill thread 1");
      }

      return NULL;
    }

    t2_ret_valid = true;
  }

  void *t1_ret = NULL;

  if (pthread_join(t1, &t1_ret) != 0) {
    warn("cannot join thread 1");
    return NULL;
  }

  if (t1_ret == NULL) {
    warnx("thread 1 returned null");

    if (t2_ret_valid) {
      hashmap_free((struct hashmap *) t2_ret);
    }

    return NULL;
  }

  if (t2_ret_valid) {
    if (merge(t1_ret, t2_ret) == NULL) {
      hashmap_free((struct hashmap *) t1_ret);
      hashmap_free((struct hashmap *) t2_ret);
      return NULL;
    }

    hashmap_free((struct hashmap *) t2_ret);
  }

  return t1_ret;
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    errx(EX_USAGE, "usage: mapred FILE N");
  }

  const char *const file = argv[1];
  const size_t n = parse_nthreads(argv[2]);

  warnx("file = %s", file);
  warnx("n threads = %lu", n);

  /* Time the input file reading. */
  struct time_info ti_file;
  ti_now(&ti_file, true);

  const size_t size = filesize(file);
  warnx("size = %lu", size);

  FILE *const fp = fopen(file, "r");

  if (fp == NULL) {
    err(EX_IOERR, "cannot open %s", file);
  }

  size_t total;
  char *const buf = readfile(fp, size, &total);

  if (fclose(fp) == EOF) {
    warn("cannot close %s", file);
  }

  if (buf == NULL) {
    warnx("only read %lu bytes", total);
    return EX_OSFILE;
  }

  warnx("read %lu bytes", total);

  ti_diff(&ti_file);
  warnx("loaded file in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
        ti_file.cpu, ti_file.rt, ti_file.wc);

  int ret = EXIT_SUCCESS;

  /* Start timing the counting. */
  struct time_info ti_all;
  ti_now(&ti_all, true);

  struct params params =
    { .buf = buf,
      .size = size,
      .depth = n,
    };

  struct hashmap *res = mapred(&params);

  const size_t n_words = hashmap_size(res);
  struct word *words = malloc(sizeof(struct word) * n_words);

  if (words == NULL) {
    ret = EX_UNAVAILABLE;
    goto end;
  }

  size_t word_idx = 0;

  /* Collect the words and their occurrences from the reduced
     hashmap. */
  for (struct hashmap_iter *iter = hashmap_iter(res);
       iter;
       iter = hashmap_iter_next(res, iter), word_idx++)
    {
      words[word_idx].ptr = word_hashmap_iter_get_key(iter);
      words[word_idx].occ = *word_hashmap_iter_get_data(iter);
    }

  /* Sort the resulting list by occurrences and word. */
  qsort(words, n_words, sizeof(struct word), (cmpfunc) countcmp);

  ti_diff(&ti_all);
  warnx("map-reduced words in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
        ti_all.cpu, ti_all.rt, ti_all.wc);

  /* Print the list of words and their occurrences. */
  for (size_t i = 0; i < n_words; i++) {
    printf("%s=%zu\n", words[i].ptr, words[i].occ);
  }

  warnx("unique words = %zu", n_words);

  /* Free the resulting words list. */
  free(words);
 end:
  hashmap_free(res);
  free(buf);
  return ret;
}
