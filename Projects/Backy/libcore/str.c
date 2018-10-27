/*
 * Copyright (c) 2015-2016, Fred Morcos <fred.morcos@gmail.com>
 *
 * Permission to  use, copy,  modify, and/or distribute  this software
 * for any  purpose with  or without fee  is hereby  granted, provided
 * that the above  copyright notice and this  permission notice appear
 * in all copies.
 *
 * THE  SOFTWARE IS  PROVIDED "AS  IS"  AND THE  AUTHOR DISCLAIMS  ALL
 * WARRANTIES  WITH  REGARD TO  THIS  SOFTWARE  INCLUDING ALL  IMPLIED
 * WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO EVENT  SHALL THE
 * AUTHOR   BE  LIABLE   FOR   ANY  SPECIAL,   DIRECT,  INDIRECT,   OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF  USE,  DATA  OR  PROFITS,  WHETHER IN  AN  ACTION  OF  CONTRACT,
 * NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,   ARISING  OUT  OF  OR  IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include "mem.h"
#include "str.h"

int xasprintf(size_t *const l, char **const s, const char *const f, ...) {
  assert(s != NULL);
  assert(f != NULL);

  va_list ap;

  int len_i = 0;
  size_t len_st = 0;

  va_start(ap, f);
  len_i = vsnprintf(NULL, 0, f, ap);
  va_end(ap);

  if (len_i < 0)
    return -1;                          /* errno is set */

  len_st = (size_t) len_i;

  if (!xrealloc((void **) s, len_st + 1, sizeof(char)))
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

/*
 * Copy string  src to buffer  dst of size dsize.   At most dsize  - 1
 * chars will be  copied.  Always NUL terminates (unless  dsize == 0).
 * Returns strlen(src); if retval >= dsize, truncation occurred.
 */
size_t xstrlcpy(char *dst, const char *src, const size_t dsize) {
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

  /* Not enough room in dst, add NUL and traverse rest of src. */
  if (nleft == 0) {
    if (dsize != 0)
      *dst = '\0';                      /* NUL-terminate dst */

    while (*src++)
      ;
  }

  assert(src - osrc >= 0);

  return ((size_t) (src - osrc) - 1);   /* count does not include NUL */
}
