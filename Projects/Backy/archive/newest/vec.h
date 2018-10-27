#pragma once

#include <stdlib.h>

typedef void (*vec_cb)(void *const);

struct vec {
  void   *ptr;                          /* pointer       */
  size_t  len;                          /* length        */
  size_t  cap;                          /* capacity      */
  size_t  chunk;                        /* chunk size    */
  size_t  esize;                        /* element size  */
  vec_cb  dtor;                         /* free callback */

#if !defined(NDEBUG)
  const char *name;                     /* name/id       */
  size_t      nreallocs;                /* reallocations */
#endif
} __attribute__((designated_init));

void vec_init(struct vec *const self,
              const char *const name,
              const size_t esize,
              const size_t chunk,
              vec_cb dtor)
  __attribute__((nonnull(1, 2)));

void vec_free(struct vec *const self)
  __attribute__((nonnull));

void *vec_add(struct vec *const self, const size_t n)
  __attribute__((malloc, warn_unused_result, nonnull));

void vec_rev(struct vec *const self, const size_t n)
  __attribute__((nonnull));

void vec_iter(struct vec *const self, vec_cb iter)
  __attribute__((nonnull));
