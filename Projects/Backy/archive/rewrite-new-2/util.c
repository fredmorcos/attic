#include <assert.h>
#include <err.h>
#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <time.h>
#include <limits.h>
#include <dirent.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#include "vec.h"
#include "util.h"

void iopbufstat(const size_t nent,
                const size_t size,
                const struct time_info *const ti) {
  assert(ti->cpu >= 0.0);
  assert(ti->rt >= 0.0);
  assert(ti->wc >= 0.0);

  char *bufsize = NULL;

  if (nent == 0) {
    assert(size == 1);
    warnx("OK: Loaded %zu entries", nent);
  } else if ((bufsize = hsize(size))) {
    warnx("OK: Loaded %zu entries into a %s buffer "
          "in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
          nent, bufsize, ti->cpu, ti->rt, ti->wc);
    free(bufsize);
  } else {
    warnx("OK: Loaded %zu entries into a %zu bytes buffer "
          "in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
          nent, size, ti->cpu, ti->rt, ti->wc);
  }
}

bool xchdir(DIR *const d, const char **const err_msg) {
  assert(d);
  assert(err_msg);

  int fd = -1;

  if ((fd = dirfd(d)) == -1) {
    *err_msg = "Could not get directory file descriptor";
    return false;
  }

  if (fchdir(fd) == -1) {
    *err_msg = "Could not change into directory";
    return false;
  }

  return true;
}

void xcldir(DIR **d) {
  if (d && *d) {
    int errno_ = errno;
    (void) closedir(*d);
    errno = errno_;
  }
}

void ptcancel_en(void) {
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
}

void ptcancel_dis(void) {
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
}

void ptsetcancel(void) {
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
}

void ptdestroy(pthread_t *t) {
  pthread_cancel(*t);
  pthread_join(*t, NULL);
}

uint ndigits(ull val) {
  uint len = 0;

  do {
    val /= 10;
    len++;
  } while (val);

  return len;
}

void ulltoa(char *str, ull val, const uint len) {
  assert(len > 0);

  double res = pow(10, len - 1);

  assert(res > 0.0);
  assert(res <= LLONG_MAX);

  ull div = (ull) llround(res);

  for (ull digit = 0; div > 0; div /= 10) {
    digit = val / div;
    *(str++) = (char) ('0' + digit);
    val -= digit * div;
  }
}

char *xrealpath(const char *const p) {
  char *rp = NULL;
  int tmp_errno = errno;

  if (!(rp = realpath(p, NULL)))
    rp = strdup(p);

  errno = tmp_errno;
  return rp;
}

void tinow(struct time_info *const ti, const bool begin) {
  assert(ti);

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

void tidiff(struct time_info *const ti) {
  assert(ti);

  time_t sec, fsec;

  tinow(ti, false);

  /* cpu time */
  ti->cpu = (ti->end_cpu - ti->begin_cpu) / (ldouble) CLOCKS_PER_SEC;
  assert(ti->cpu >= 0.0);

  /* real-time */
  if (ti->begin_rt.tv_nsec > ti->end_rt.tv_nsec) {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec - 1;
    fsec = ti->end_rt.tv_nsec + 1000000000L - ti->begin_rt.tv_nsec;
  } else {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec;
    fsec = ti->end_rt.tv_nsec - ti->begin_rt.tv_nsec;
  }

  ti->rt = sec + (ldouble) 1.e-9 * fsec;
  assert(ti->rt >= 0.0);

  /* wall-clock time */
  if (ti->begin_wc.tv_usec > ti->end_wc.tv_usec) {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec - 1;
    fsec = ti->end_wc.tv_usec + 1000000 - ti->begin_wc.tv_usec;
  } else {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec;
    fsec = ti->end_wc.tv_usec - ti->begin_wc.tv_usec;
  }

  ti->wc = sec + (ldouble) 1.e-6 * fsec;
  assert(ti->wc >= 0.0);
}

char *hsize(ldouble size) {
  static const char units[] = "BKMGTPE";
  static const size_t units_len = sizeof(units);

  char *res = NULL;
  size_t i = 0;

  while (size >= 1024.0 && i < units_len) {
    size /= 1024.0;
    i++;
  }

  return xasprintf(NULL, &res, "%.2Lf%c", size, units[i])
    < 0 ? NULL : res;
}

int xasprintf(size_t *const l, char **const s, const char *const f, ...) {
  va_list ap;

  int len_i = 0;
  size_t len_st = 0;

  va_start(ap, f);
  len_i = vsnprintf(NULL, 0, f, ap);
  va_end(ap);

  if (len_i < 0)
    return -1;                          /* errno is set */

  len_st = (size_t) len_i;

  if (!(*s = xrealloc(NULL, len_st + 1, sizeof(char))))
    return -2;                          /* errno is set */

  va_start(ap, f);
  len_i = vsnprintf(*s, len_st + 1, f, ap);
  va_end(ap);

  if (len_i < 0 || (size_t) len_i != len_st) {
    free(*s);
    return -1;                          /* errno is set */
  }

  if (l != NULL)
    *l = len_st;

  return 0;
}

void *xrealloc(void *optr, const size_t nmemb, const size_t size) {
  /*
   * This is sqrt(SIZE_MAX + 1), as s1 * s2 <= SIZE_MAX
   *
   * if both s1 < MUL_NO_OVERFLOW and s2 < MUL_NO_OVERFLOW
   */
#define MUL_NO_OVERFLOW	((size_t)1 << (sizeof(size_t) * 4))

  if ((nmemb >= MUL_NO_OVERFLOW || size >= MUL_NO_OVERFLOW) &&
      nmemb > 0 && SIZE_MAX / nmemb < size) {
    errno = ENOMEM;
    return NULL;
  }

  return realloc(optr, size * nmemb);
}

/*
 * Copy string  src to buffer  dst of size dsize.   At most dsize  - 1
 * chars will be  copied.  Always NUL terminates (unless  dsize == 0).
 * Returns strlen(src); if retval >= dsize, truncation occurred.
 */
size_t strlcpy(char *dst, const char *src, const size_t dsize) {
  const char *osrc = src;
  size_t nleft = dsize;

  /* Copy as many bytes as will fit. */
  if (nleft != 0) {
    while (--nleft != 0) {
      if ((*dst++ = *src++) == '\0')
        break;
    }
  }

  /* Not enough room in dst, add NUL and traverse rest of src. */
  if (nleft == 0) {
    if (dsize != 0)
      *dst = '\0';		/* NUL-terminate dst */

    while (*src++)
      ;
  }

  assert(src - osrc >= 0);

  return ((size_t) (src - osrc) - 1);	/* count does not include NUL */
}
