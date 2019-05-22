/* mapred.c - map-reduce to count the occurrences of words in a file
 * Frederic-Gerald Morcos (fredmorcos) <fred.morcos@gmail.com>
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <err.h>
#include <sysexits.h>
#include <stdbool.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <pthread.h>
#include <ctype.h>
#include <signal.h>

/*
 * building and running:
 *
 * simply typing `make` will build a whole bunch of variants of this
 * program. a release build in `mapred`, a debug build in
 * `mapred_debug`, a build with pointer checking enabled in
 * `mapred_ptrcheck` and sanitizer builds in different `mapred_san*`
 * executables.
 *
 * note that `make` will build with the default system compiler: cc,
 * to use clang run `CC=clang make` and to use gcc run `CC=gcc make`
 *
 * `make clean` will take care of removing those and their respective
 * object files.
 *
 * to run: ./mapred FILE N_THREADS
 * with valgrind: valgrind ./mapred_debug FILE N_THREADS
 *
 * afl:
 *
 * to build with afl use `CC=afl-clang make` or `CC=afl-gcc make`.
 * to run in parallel, in separate terminals:
 *   afl-fuzz -t 500 -i tests.in -o afl -M f1 ./mapred @@ 5
 *   afl-fuzz -t 500 -i tests.in -o afl -S f2 ./mapred @@ 5
 *   afl-fuzz -t 500 -i tests.in -o afl -S f3 ./mapred @@ 5
 *   afl-fuzz -t 500 -i tests.in -o afl -S f4 ./mapred @@ 5
 *
 * notes:
 *
 * - vectors might be more efficient for the book-keeping of lists of
 *   words.
 *
 * - self-balancing trees, or a hashmap, would probably be more
 *   efficient than arrays and vectors for the book-keeping of lists
 *   of words.
 *
 * - i explicitly avoided VLAs due to being dangerous and i think also
 *   slower in some cases.
 *
 * - i am unsure on how punctuation, spacing, upper and lowercase
 *   would be handled for other languages.
 *
 * - this will not work for composed words (with a dash in
 *   between). foo-bar will be handled as two words `foo` and `bar`
 *   instead of a single word `foobar`.
 *
 * - this assumes that case does not matter: `Foo` and `foo` are the
 *   same.
 */

struct word {
  char *ptr;                    /* nul-terminated string */
  size_t n;                     /* number of occurrences */
};

struct params {
  char *ptr;                    /* nul-terminated buffer */
  struct word *words;           /* list of accumulated words */
  size_t n;                     /* number of words */
  bool err;                     /* whether an error occurred */
  int err_no;                   /* thread-local errno */
};

/* for casting function pointers */
typedef int (*cmpfunc)(const void *, const void *);
typedef void *(*tfunc)(void *);

/* prototypes */
int wordcmp(const struct word *w1, const struct word *w2);
int countcmp(const struct word *w1, const struct word *w2);
struct params *count(struct params *const p);
struct params *reduce(const size_t n, struct params *const ps);

/* compare two words according to their text */
int wordcmp(const struct word *w1, const struct word *w2) {
  return strcmp(w1->ptr, w2->ptr);
}

/* compare two words according to their occurrences */
int countcmp(const struct word *w1, const struct word *w2) {
  /* since we're dealing with size_t we cannot subtract arbitrarily,
   * so instead we do each check explicitly
   */
  if (w1->n == w2->n) {
    /* here we `return 0` instead of `return wordcmp(w1, w2)` because
     * the list should already be sorted by word (for the bsearch()
     * calls)
    */
    return 0;
  } else if (w1->n > w2->n) {
    return -1;
  } else {
    return 1;
  }
}

struct params *count(struct params *const p) {
  while (true) {
    /* nullify any spaces and punctuation */
    while (*p->ptr != '\0' && !isalnum((unsigned char) *p->ptr)) {
      *p->ptr = '\0';
      p->ptr++;
    }

    /* if we stumbled on the end of the buffer, then exit */
    if (*p->ptr == '\0') {
      break;
    }

    /* otherwise, we've found the start of a word */
    const struct word w = { .ptr = p->ptr, .n = 1 };

    /* consume the characters of the word */
    while (isalnum((unsigned char) *p->ptr)) {
      /* we lower all words, assuming `Lorem` and `lorem` are the
       * same.
       */
      *p->ptr = (char) tolower(*p->ptr);
      p->ptr++;
    }

    /* we're here because we reached a non-letter/digit, if the
     * current character is the end of the buffer, then we are at the
     * last word and should not increment the pointer, otherwise mark
     * the end of the current word with a nul-termination and
     * increment the pointer to move onto the next word with the next
     * iteration
     */
    if (*p->ptr != '\0') {
      *p->ptr = '\0';
      p->ptr++;
    }

    /* here we will refer to an already existing copy of the word in
     * `w` (the one we just uncovered) in case we can find it in the
     * accumulated list of words
     */
    struct word *found = NULL;

    if (p->words != NULL) {
      /* list of words is not empty, do the search */
      found = bsearch(&w.ptr,
                      p->words,
                      p->n,
                      sizeof(struct word),
                      (cmpfunc) wordcmp);
    }

    if (found != NULL) {
      /* increment the occurrence of the instance in the list and move
       * on to reading the next word from the buffer
       */
      found->n += 1;
      continue;
    }

    /* if the word was not found in the list (has not been seen
     * before), allocate space for an extra element in the list and
     * add the word referred to by `w` to it. an array is not ideal,
     * so a pre-allocating vector would amortize the cost of
     * reallocations (however the operating system still preallocates
     * in page sizes so that does amortize the cost to a certain
     * degree)
     *
     * https://github.com/fredmorcos/rdiff/blob/master/vec.h
     * https://github.com/fredmorcos/rdiff/blob/master/vec.c
     */
    struct word *const new = (struct word *)
      realloc(p->words, (p->n + 1) * sizeof(struct word));

    if (new == NULL) {
      p->err = true;
      p->err_no = errno;
      return NULL;
    }

    p->words = new;
    memcpy(&p->words[p->n], &w, sizeof(struct word));
    p->n++;

    /* with a new element in the list, resort it by words to take
     * advantage of the bsearch() lookup. maybe insertion sort would
     * be more efficient
     */
    qsort(p->words, p->n, sizeof(struct word), (cmpfunc) wordcmp);
  }

  return p;
}

struct params *reduce(const size_t n, struct params *const ps) {
    /* the `reduce` phase: we will use the first accumulated list as the
   * result, so sort it by word to be able to use bsearch() on it
   */
  qsort(ps[0].words, ps[0].n, sizeof(struct word), (cmpfunc) wordcmp);

  /* go over the accumulated lists from each chunk (except list 0) */
  for (size_t i = 1; i < n; i++) {
    /* go over each word in the list */
    for (size_t j = 0; j < ps[i].n; j++) {
      /* try to find the word in the result list (list 0) */
      struct word *const found = bsearch(&ps[i].words[j].ptr,
                                         ps[0].words,
                                         ps[0].n,
                                         sizeof(struct word),
                                         (cmpfunc) wordcmp);

      if (found != NULL) {
        /* if we found the word, increment its count by the
         * occurrences of the new word
         */
        found->n += ps[i].words[j].n;
      } else {
        /* if we didnt find it, then it is a new word, make space for
         * it and insert it into the result list, then sort the
         * list by words for use with bsearch()
         */
        struct word *const new =
          realloc(ps[0].words, (ps[0].n + 1) * sizeof(struct word));

        if (new == NULL) {
          warn("cannot allocate space for result list");
          return NULL;
        }

        ps[0].words = new;
        memcpy(&ps[0].words[ps[0].n], &ps[i].words[j], sizeof(struct word));
        ps[0].n++;

        qsort(ps[0].words, ps[0].n, sizeof(struct word), (cmpfunc) wordcmp);
      }
    }
  }

  /* when we're done reducing, sort the list by occurrences */
  qsort(ps[0].words, ps[0].n, sizeof(struct word), (cmpfunc) countcmp);

  return ps;
}

int main(int argc, char *argv[]) {
  if (argc != 3) {
    errx(EX_USAGE, "usage: mapred FILE N");
  }

  const char *const file = argv[1];
  const char *const n_str = argv[2];
  const char *const n_str_end = n_str + strlen(n_str);
  char *endptr = NULL;

  errno = 0;
  const long n_tmp = strtol(n_str, &endptr, 10);

  if (errno != 0 || endptr == n_str || endptr != n_str_end) {
    err(EX_USAGE, "cannot parse number of threads");
  }

  if (n_tmp <= 0) {
    errx(EX_USAGE, "invalid number of threads");
  }

  const size_t n = (size_t) n_tmp;

#if !defined(NDEBUG)
  warnx("file = %s", file);
  warnx("n = %lu", n);
#endif

  struct stat st;

  if (stat(file, &st) == -1) {
    err(EX_OSFILE, "cannot stat %s", file);
  }

  if (st.st_size < 0) {
    errx(EX_OSFILE, "invalid filesize %ld", st.st_size);
  }

  if (st.st_size == 0) {
    errx(EX_OSFILE, "file has zero length");
  }

  const size_t size = (size_t) st.st_size;

#if !defined(NDEBUG)
  warnx("size = %lu", size);
#endif

  /* here i decided to just read the entire file at once, for huge
   * files i guess mmap() would be better
   */
  char *const buf = malloc(size + 1);

  if (buf == NULL) {
    err(EX_UNAVAILABLE, "cannot allocate space to load file");
  }

  int ret = EXIT_SUCCESS;

  FILE *const fp = fopen(file, "r");

  if (fp == NULL) {
    warn("cannot open %s", file);
    ret = EX_OSFILE;
    goto end_buf;
  }

  size_t total = 0;

  while (true) {
    const size_t read = fread(buf + total, 1, size - total, fp);

    total += read;

    assert(total <= size);

    if (total < size) {
      if (ferror(fp) != 0) {
        warn("cannot read from %s", file);
        goto end_fp;
      }

      if (feof(fp) != 0) {
        break;
      }

      continue;
    }

    break;
  }

  buf[size] = '\0';

#if !defined(NDEBUG)
  warnx("read %lu bytes", total);
#endif

  /* allocate and zero-out parameters list, this needs to be zeroed
   * out to make sure the `words` pointer is NULL, so if an error
   * occurs during execution, the cleanup happening at the `end_words`
   * label can reliably free those pointers that are not NULL
   */
  struct params *const ps = calloc(n, sizeof(struct params));

  if (ps == NULL) {
    warn("cannot allocate space for parameters");
    ret = EX_UNAVAILABLE;
    goto end_fp;
  }

  pthread_t *const ts = malloc(n * sizeof(pthread_t));

  if (ts == NULL) {
    warn("cannot allocate space for threads");
    ret = EX_UNAVAILABLE;
    goto end_params;
  }

  /* the `map` phase */
  char *chunk = buf;
  size_t total_chunksize = 0;
  const size_t chunksize = size / n;

  /* count the number of threads we've created at any point, since we
   * may not actually create N threads in the end due to chunk having
   * long enough words that would push the last X chunks to have zero
   * size
   */
  size_t nts = 0;

  for (size_t i = 0; i < n; i++, nts++) {
    if (total_chunksize > size) {
      break;
    }

    /* needed to make chunk sizes variable for each thread */
    size_t real_chunksize = chunksize;

    if (total_chunksize + chunksize > size) {
      real_chunksize = size - total_chunksize;
    }

    /* if the end of the chunk lands in the middle of the word,
     * include that word in the chunk
     */
    while (isalnum(chunk[real_chunksize])) {
      real_chunksize++;
    }

    /* mark the end of the word */
    chunk[real_chunksize] = '\0';
    real_chunksize++;

    ps[i] = (struct params) { .ptr = chunk };

    if (pthread_create(&ts[i], NULL, (tfunc) count, &ps[i]) != 0) {
      warnx("cannot create thread");

      /* kill the threads we've created so far */
      for (size_t t = 0; t < i; t++) {
        if (pthread_kill(ts[t], SIGKILL) != 0) {
          warnx("cannot kill thread");
        }
      }

      ret = EX_OSERR;
      goto end_threads;
    }

    /* move the chunk pointer to the beginning of the next chunk */
    chunk += real_chunksize;
    total_chunksize += real_chunksize;
  }

#if !defined(NDEBUG)
  warnx("created %zu threads", nts);
#endif

  /* any errors with joining all of the threads */
  bool err = false;

  for (size_t t = 0; t < nts; t++) {
    if (pthread_join(ts[t], NULL) != 0) {
      warn("cannot join thread");
      err = true;
    }

    if (ps[t].err) {
      warnx("error occurred when counting: %s", strerror(ps[t].err_no));
      err = true;
    }
  }

  if (err) {
    ret = EX_OSERR;
    goto end_threads;
  }

  /* the resulting list will be in ps[0].words */
  if (reduce(n, ps) == NULL) {
    ret = EX_UNAVAILABLE;
    goto end_threads;
  }

  /* print the list of words and their occurrences */
  for (size_t i = 0; i < ps[0].n; i++) {
    printf("%s=%zu\n", ps[0].words[i].ptr, ps[0].words[i].n);
  }

 end_threads:
  free(ts);

  /* free word lists that have been allocated */
  for (size_t i = 0; i < n; i++) {
    if (ps[i].words != NULL) {
      free(ps[i].words);
    }
  }
 end_params:
  free(ps);
 end_fp:
  if (fclose(fp) == EOF) {
    warn("cannot close %s", file);
  }
 end_buf:
  free(buf);
  return ret;
}
