#include <assert.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include "mem.h"
#include "str.h"

size_t xstrlcpy(char *dst,
                const char *src,
                const size_t dsize) {
  /* String  copy  from  OpenBSD: Copy  string  src  to
   * buffer dst of size dsize.  At most dsize - 1 chars
   * will  be copied.   Always  NUL terminates  (unless
   * dsize ==  0).  Returns  strlen(src); if  retval >=
   * dsize, truncation occurred.
   */
  assert(dst != NULL);
  assert(src != NULL);

  const char *osrc = src;
  size_t nleft = dsize;

  /* Copy as many bytes as will fit. */
  if (nleft != 0) {
    while (--nleft != 0) {
      if ((*dst++ = *src++) == '\0')
        break;
    }
  }

  /* Not enough room in dst,  add NUL and traverse rest
   * of src.
   */
  if (nleft == 0) {
    /* NUL-terminate dst */
    if (dsize != 0)
      *dst = '\0';

    while (*src++)
      ;
  }

  assert(src - osrc >= 0);

  /* count does not include NUL */
  return ((size_t) (src - osrc) - 1);
}

int xasprintf(size_t *const l,
              char **const s,
              const char *const f, ...) {
  assert(s != NULL);
  assert(f != NULL);

  va_list ap;

  int len_i = 0;
  size_t len_st = 0;

  void *new_p = NULL;

  va_start(ap, f);
  len_i = vsnprintf(NULL, 0, f, ap);
  va_end(ap);

  /* errno is set */
  if (len_i < 0)
    return (-1);

  len_st = (size_t) len_i;

  new_p = xrealloc((void **) s, len_st + 1, sizeof(char));

  /* errno is set */
  if (new_p == NULL)
    return (-2);

  va_start(ap, f);
  len_i = vsnprintf(*s, len_st + 1, f, ap);
  va_end(ap);

  /* errno is set */
  if (len_i < 0 || (size_t) len_i != len_st) {
    free(*s);
    return (-1);
  }

  if (l != NULL)
    *l = len_st;

  return (0);
}

char *humansize(long double size) {
  assert(size > 0);

  static const char u[] = "BKMGTPE";

  char *res = NULL;
  size_t i = 0;
  int ret = 0;

  while (size >= 1024 && i < sizeof(u)) {
    size /= 1024;
    i++;
  }

  ret = xasprintf(NULL, &res, "%.2Lf%c", size, u[i]);

  if (ret < 0)
    return (NULL);

  return (res);
}

bool strchars(const char *const buf,
              const size_t len,
              const char ch,
              const size_t num) {
  assert(buf != NULL);
  assert(num > 0);

  for (size_t i = 0, c = 0; i < len; i++) {
    if ((c = buf[i] == ch ? c + 1 : 0) == num)
      return (true);
  }

  return (false);
}
