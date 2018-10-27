#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <bsd/bsd.h>
#include "util.h"

double timediff(clock_t c) {
  return (double) (clock() - c) / (double) CLOCKS_PER_SEC;
}

__attribute__((format (printf, 3, 4)))
int asprintf(size_t *const len,
             char **const str,
             const char *const fmt, ...) {
  va_list ap;

  int    len_i  = 0;
  size_t len_st = 0;

  va_start(ap, fmt);
  len_i = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);

  if (len_i < 0)
    return -1;                          /* errno is set */

  len_st = (size_t) len_i;
  *str = reallocarray(NULL, len_st + 1, sizeof(char));

  if (*str == NULL)
    return -2;                          /* errno is set */

  va_start(ap, fmt);
  len_i = vsnprintf(*str, len_st + 1, fmt, ap);
  va_end(ap);

  if (len_i < 0 || (size_t) len_i != len_st) {
    free(*str);
    return -1;                          /* errno is set */
  }

  if (len != NULL)
    *len = len_st;

  return 0;
}

char *humansize(double size) {
  const char   units[]   = "BKMGTPE";
  const size_t units_len = sizeof(units);

  char   *res = NULL;
  size_t  i   = 0;

  while (size >= 1024.0 && i < units_len) {
    size /= 1024.0;
    i++;
  }

  return asprintf(NULL, &res, "%.2f%c", size, units[i])
    < 0 ? NULL : res;
}

__attribute__((pure))
bool find_nchars(const char *const buf,
                 const size_t len,
                 const char ch,
                 const size_t num) {
  for (size_t i = 0, c = 0; i < len; i++) {
    c = buf[i] == ch ? c + 1 : 0;

    if (c == num)
      return true;
  }

  return false;
}
