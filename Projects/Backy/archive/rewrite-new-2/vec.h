#pragma once

#include <stdbool.h>
#include <stdlib.h>

typedef void (vec_cb)(void *const);

struct vec {
  void   *ptr;
  size_t  len;
  size_t  cap;
  size_t  chunk;
  size_t  esize;
  vec_cb *free_cb;
};

void  vec_init(struct vec *const self,
               const size_t chunk,
               const size_t esize,
               vec_cb *const free_cb);
void  vec_free(struct vec *const self);
void  vec_revn(struct vec *const self, const size_t n);
bool  vec_extn(struct vec *const self, const size_t n);
void *vec_addn(struct vec *const self, const size_t n)
  __attribute__((malloc, warn_unused_result));
