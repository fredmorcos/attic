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
#include <errno.h>
#include <stdlib.h>
#include <inttypes.h>
#include "mem.h"

/*
 * Taken from OpenBSD reallocarray:
 *
 * This is sqrt(SIZE_MAX + 1), since (s1  * s2) is <= SIZE_MAX if both
 * s1 and s2 are < MUL_NO_OFLOW.
 */
#define MUL_NO_OFLOW ((size_t) 1 << (sizeof(size_t) * 4))

void *xrealloc(void **p, const size_t n, const size_t size) {
  assert(p != NULL);
  assert(n > 0);
  assert(size > 0);

  void *new_p;

  if ((n >= MUL_NO_OFLOW || size >= MUL_NO_OFLOW) &&
      n > 0 && SIZE_MAX / n < size) {
    errno = EOVERFLOW;
    return NULL;
  }

  if (!(new_p = realloc(*p, n * size)))
    return NULL;

  return (*p = new_p);
}
