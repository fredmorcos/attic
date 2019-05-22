/* mapred.c - Map-Reduce to count the occurrences of words in a file */
/* Frederic-Gerald Morcos (fredmorcos) <fred.morcos@gmail.com> */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <sysexits.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <ctype.h>
#include <signal.h>
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

/* Convenience structure for keeping a word and its occurrences. */
struct word {
  const char *ptr;             /* Pointer to nul-terminated string. */
  size_t occ;                  /* Number of occurrences. */
};

/* Structure for keeping parameters for the threaded count
   function. */
struct count_params {
  struct hashmap *words;       /* Words list in a hashmap. */
  char *buf_ptr;               /* Pointer to nul-terminated buffer. */
  int tl_errno;                /* Thread-local errno. */
};

/* Helpers for casting function pointers. */
typedef int (*cmpfunc)(const void *, const void *);
typedef void *(*tfunc)(void *);

/* Prototypes. */
int countcmp(const struct word *w1, const struct word *w2);
struct count_params *count(struct count_params *const params);
struct hashmap *merge(struct hashmap *const m1, const struct hashmap *const m2);

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
struct count_params *count(struct count_params *const params) {
  typedef unsigned char uchar;

  /* Allocate the hashmap. */
  params->words = malloc(sizeof(struct hashmap));

  if (params->words == NULL) {
    return NULL;
  }

  /* Initialize the hashmap. */
  hashmap_init(params->words, hashmap_hash_string, hashmap_compare_string, 10000);

  while (true) {
    /* Nullify any spaces and punctuation. */
    while (*params->buf_ptr != '\0' && !isalnum((uchar) *params->buf_ptr)) {
      *params->buf_ptr = '\0';
      params->buf_ptr++;
    }

    /* Stumbled onto the end of the buffer, so exit. */
    if (*params->buf_ptr == '\0') {
      break;
    }

    /* Found the start of a word. */
    const char *const w = params->buf_ptr;

    /* Consume the characters of the word and lowercase them. */
    while (isalnum((uchar) *params->buf_ptr)) {
      *params->buf_ptr = (char) tolower(*params->buf_ptr);
      params->buf_ptr++;
    }

    /* Reached a non-alphanumeric: If the current character is the end
       of the buffer, then the last word has been traversed and the
       buffer pointer must not be incremented, otherwise mark the end
       of the current word with a nul-termination and increment the
       pointer to move onto the next word at the next iteration. */
    if (*params->buf_ptr != '\0') {
      *params->buf_ptr = '\0';
      params->buf_ptr++;
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
    size_t *res = word_hashmap_put(params->words, w, occ);

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

  return params;
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

/* Structure for keeping parameters to the threaded recude
   function. */
struct reduce_params {
  struct count_params *const ps; /* Count parameters. */
  const size_t n;                /* Number of count parameters. */
};

/* Main helper prototypes. */
size_t parse_nthreads(const char *const str);
size_t filesize(const char *const fn);
char *readfile(FILE *const fp, size_t size, size_t *const total);
size_t *map(pthread_t *const ts,
            struct count_params *const ps,
            const size_t n,
            char *const buf,
            const size_t size,
            size_t *const nts);
struct hashmap *reduce(struct reduce_params *const rps);

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

/* Mult-threaded map, split the input buffer `buf` into chunks, spawn
   threads from `ts` to count the word occurrences in each chunk, and
   return the number of threads created. */
size_t *map(pthread_t *const ts,
            struct count_params *const ps,
            const size_t n,
            char *const buf,
            const size_t size,
            size_t *const nts)
{
  /* the `map` phase */
  char *chunk = buf;
  size_t total_chunksize = 0;
  const size_t chunksize = size / n;

  /* Count the number of threads we're creating, since we may not
     actually create N threads in the end due to chunk having long
     enough words that would push the last X chunks to have zero size
     and the last chunk having a size smaller than all the others. */
  *nts = 0;

  for (size_t i = 0; i < n; i++, (*nts)++) {
    if (total_chunksize > size) {
      break;
    }

    /* Needed to make chunk sizes variable for each thread. */
    size_t real_chunksize = chunksize;

    if (total_chunksize + chunksize > size) {
      real_chunksize = size - total_chunksize;
    }

    /* If the end of the chunk lands in the middle of the word,
       include that word in the chunk. */
    while (isalnum(chunk[real_chunksize])) {
      real_chunksize++;
    }

    /* Mark the end of the word. */
    chunk[real_chunksize] = '\0';
    real_chunksize++;

    ps[i] = (struct count_params) { .buf_ptr = chunk };

    if (pthread_create(&ts[i], NULL, (tfunc) count, &ps[i]) != 0) {
      warnx("cannot create thread");

      /* Kill the threads we've created so far. */
      for (size_t t = 0; t < i; t++) {
        if (pthread_kill(ts[t], SIGKILL) != 0) {
          warnx("cannot kill thread");
        }
      }

      return NULL;
    }

    /* Move the chunk pointer to the beginning of the next chunk. */
    chunk += real_chunksize;
    total_chunksize += real_chunksize;
  }

  return nts;
}

/* The reduction step. Recursively split the list of hashmaps until
   there are either 2 or 3 remaining, in which case merge them
   manually. Each recursive call is done in parallel and a "sync" step
   (two joins) are made before the caller returns. Returns the hashmap
   which contains the resulting merge, so that it can - in turn - be
   merged with its siblings in the parent. */
struct hashmap *reduce(struct reduce_params *const rps) {
  if (rps->n >= 4) {
    pthread_t t1;
    struct reduce_params t1_params =
      { .ps = rps->ps, .n = rps->n / 2 };

    if (pthread_create(&t1, NULL, (tfunc) reduce, &t1_params) != 0) {
      warnx("cannot create reduce thread 1");
      return NULL;
    }

    pthread_t t2;
    struct reduce_params t2_params =
      { .ps = rps->ps + rps->n / 2, .n = rps->n - rps->n / 2 };

    if (pthread_create(&t2, NULL, (tfunc) reduce, &t2_params) != 0) {
      warnx("cannot create reduce thread 2");

      if (pthread_kill(t1, SIGKILL) != 0) {
        warnx("cannot kill reduce thread 1");
      }

      return NULL;
    }

    void *t1_ret;

    if (pthread_join(t1, &t1_ret) != 0) {
      warn("cannot join reduce thread 1");
      return NULL;
    }

    void *t2_ret;

    if (pthread_join(t2, &t2_ret) != 0) {
      warn("cannot join reduce thread 2");
      return NULL;
    }

    return merge(t1_ret, t2_ret);
  }

  if (rps->n >= 2) {
    if (merge(rps->ps[0].words, rps->ps[1].words) == NULL) {
      return NULL;
    }

    if (rps->n == 3) {
      if (merge(rps->ps[0].words, rps->ps[2].words) == NULL) {
        return NULL;
      }
    }
  }

  return rps->ps[0].words;
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
  struct time_info ti_map;
  struct time_info ti_all;
  ti_now(&ti_map, true);
  ti_now(&ti_all, true);

  /* Allocate and zero-out the parameters list, this needs to be
     zeroed to make sure the `words` pointer is NULL, so if an error
     occurs during execution, the cleanup happening at the `end_words`
     label can reliably free those pointers that are not NULL. */
  struct count_params *const ps = calloc(n, sizeof(struct count_params));

  if (ps == NULL) {
    warn("cannot allocate space for parameters");
    ret = EX_UNAVAILABLE;
    goto end_buf;
  }

  /* Allocate threads. */
  pthread_t *const ts = malloc(n * sizeof(pthread_t));

  if (ts == NULL) {
    warn("cannot allocate space for threads");
    ret = EX_UNAVAILABLE;
    goto end_params;
  }

  size_t nts = 0;

  if (map(ts, ps, n, buf, size, &nts) == NULL) {
    goto end_threads;
  }

  warnx("created %zu threads", nts);

  /* Any errors when joining the threads. */
  bool err = false;

  for (size_t t = 0; t < nts; t++) {
    if (pthread_join(ts[t], NULL) != 0) {
      warn("cannot join thread");
      err = true;
    }

    if (ps[t].tl_errno != 0) {
      warnx("error occurred when counting: %s", strerror(ps[t].tl_errno));
      err = true;
    }
  }

  if (err) {
    ret = EX_OSERR;
    goto end_threads;
  }

  ti_diff(&ti_map);
  warnx("mapped words in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
        ti_map.cpu, ti_map.rt, ti_map.wc);

  struct time_info ti_red;
  ti_now(&ti_red, true);

  struct hashmap *res = reduce(&(struct reduce_params) { .ps = ps, .n = nts });

  if (res == NULL) {
    ret = EX_UNAVAILABLE;
    goto end_threads;
  }

  const size_t n_words = hashmap_size(res);
  struct word *words = malloc(sizeof(struct word) * n_words);

  if (words == NULL) {
    ret = EX_UNAVAILABLE;
    goto end_threads;
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

  ti_diff(&ti_red);
  warnx("reduced words in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
        ti_red.cpu, ti_red.rt, ti_red.wc);

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
 end_threads:
  free(ts);

  /* Free word lists that have been allocated. */
  for (size_t i = 0; i < nts; i++) {
    if (ps[i].words == NULL) {
      break;
    }

    for (struct hashmap_iter *iter = hashmap_iter(ps[i].words);
         iter;
         iter = hashmap_iter_next(ps[i].words, iter))
      {
        free(word_hashmap_iter_get_data(iter));
      }

    hashmap_destroy(ps[i].words);
    free(ps[i].words);
  }
 end_params:
  free(ps);
 end_buf:
  free(buf);
  return ret;
}
