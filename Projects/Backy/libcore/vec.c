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
#include <stdint.h>
#include <string.h>
#include "mem.h"
#include "vec.h"

#ifdef DEBUG
#include <err.h>
#include "time.h"
#endif

void vec_init(struct vec *const self,
              const size_t chunk,
              const size_t esize,
              vec_cb *const free_cb) {
  assert(self != NULL);
  assert(chunk > 0);
  assert(esize > 0);

  (void) memset(self, 0, sizeof(struct vec));

  self->ptr = NULL;
  self->len = 0;
  self->cap = 0;
  self->chunk = chunk;
  self->esize = esize;
  self->free_cb = free_cb;
}

void vec_free(struct vec *const self) {
  assert(self != NULL);

#ifdef DEBUG
  warnx("DEBUG: %s:%d", __FILE__, __LINE__);
  warnx(" Element: %zu bytes, Chunk: %zu elems", self->esize, self->chunk);
  warnx(" Free (length/capacity): %p", self->ptr);
  warnx("   Final: %zu/%zu", self->len, self->cap);
#endif

  if (self->free_cb) {
    uint8_t *p = self->ptr;

    for (size_t i = 0; i < self->len; i++, p += self->esize)
      self->free_cb(p);
  }

  if (!self->ptr) {
    assert(self->len == 0);
    assert(self->cap == 0);
    return;
  }

  free(self->ptr);
}

void *vec_addn(struct vec *const self, const size_t n) {
  assert(self != NULL);
  assert(n > 0);
  assert(self->len <= self->cap);

  void *new_ptr;

  if (self->len + n > self->cap) {
    size_t new_cap = 0;

#ifdef DEBUG
    struct time_info ti;
#endif

    if (n > self->cap) {
      if (self->cap > SIZE_MAX - n)
        goto err_oflow;
      new_cap = self->cap + n;
    } else {
      if (self->cap > SIZE_MAX / 2)
        goto err_oflow;
      new_cap = self->cap * 2;
    }

    new_cap = new_cap > self->chunk ? new_cap : self->chunk;

#ifdef DEBUG
    ti_now(&ti, true);
#endif

    if (!(new_ptr = xrealloc(self->ptr, new_cap, self->esize)))
      return NULL;

#ifdef DEBUG
    ti_diff(&ti);
    warnx("DEBUG: %s:%d", __FILE__, __LINE__);
    warnx(" %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc", ti.cpu, ti.rt, ti.wc);
    /* XXX: Accessing self->ptr here may trigger undefined behavior. */
    warnx(" Reallocation (length/capacity): %p → %p", self->ptr, new_ptr);
    warnx("  Need: %zu elements", n);
    warnx("  Old:  %zu/%zu", self->len, self->cap);
    warnx("  New:  %zu/%zu", self->len + n, new_cap);
#endif

    /* XXX: Accessing self->ptr here may trigger undefined behavior. */
    self->ptr = new_ptr;
    self->cap = new_cap;
  }

  self->len += n;

  return (uint8_t *) self->ptr + (self->len - n) * self->esize;

 err_oflow:
  errno = EOVERFLOW;
  return NULL;
}

bool vec_extn(struct vec *const self, const size_t n) {
  assert(self != NULL);
  assert(n > 0);

  return vec_addn(self, n) == NULL ? false : true;
}

void vec_revn(struct vec *const self, const size_t n) {
  assert(self != NULL);
  assert(n > 0);
  assert(self->len >= n);

  self->len -= n;
}
