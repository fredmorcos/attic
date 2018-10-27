#pragma once
#define attr __attribute__

#include <stdlib.h>

typedef void (*VecCB)(void *const);

struct vec {
  void   *ptr;                      /* pointer       */
  size_t  len;                      /* length        */
  size_t  cap;                      /* capacity      */
  size_t  chunk;                    /* chunk size    */
  size_t  esize;                    /* element size  */
  VecCB   dtor;                     /* free callback */
} attr((designated_init));

typedef struct vec Vec;

#define AutoVec \
  attr((cleanup(vec_free))) Vec

Vec vec_init(const size_t esize,
             const size_t chunk,
             VecCB dtor);

void vec_free(Vec *const self)
  attr((nonnull));

void *vec_add(Vec *const self, const size_t n)
  attr((malloc, warn_unused_result, nonnull));

void vec_rev(Vec *const self, const size_t n)
  attr((nonnull));

void vec_iter(Vec *const self, VecCB iter)
  attr((nonnull));
