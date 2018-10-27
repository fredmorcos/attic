#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include "vec.h"
#include "mem.h"

Vec vec_init(const size_t esize,
             const size_t chunk,
             VecCB dtor) {
  assert(esize > 0);
  assert(chunk > 0);

  Vec self;

  (void) memset(&self, 0, sizeof(Vec));

  self.ptr = NULL;
  self.len = 0;
  self.cap = 0;
  self.chunk = chunk;
  self.esize = esize;
  self.dtor = dtor;

  return (self);
}

void vec_free(Vec *const self) {
  assert(self != NULL);

  if (self->dtor != NULL)
    vec_iter(self, self->dtor);

  if (self->ptr == NULL) {
    assert(self->len == 0);
    assert(self->cap == 0);
  } else {
    assert(self->cap > 0);
    free(self->ptr);
    self->ptr = NULL;
    self->len = 0;
    self->cap = 0;
    self->dtor = NULL;
  }
}

void *vec_add(Vec *const self, const size_t n) {
  assert(self != NULL);
  assert(self->len <= self->cap);

  void *new_ptr = NULL;

  if (SIZE_MAX - n < self->len)
    goto err_overflow;

  if (self->len + n > self->cap) {
    if (SIZE_MAX / 2 < self->cap)
      goto err_overflow;

    size_t new_cap = 0;

    new_cap = (self->len + n) > (self->cap * 2) ?
      (self->len + n) : (self->cap * 2);
    new_cap = new_cap > self->chunk ?
      new_cap : self->chunk;

    assert(new_cap >  self->cap);
    assert(new_cap >= self->len + n);

    new_ptr = xrealloc(&self->ptr,
                       new_cap,
                       self->esize);

    if (new_ptr == NULL)
      return (NULL);

    self->ptr = new_ptr;
    self->cap = new_cap;
  }

  self->len += n;

  return ((uint8_t *) self->ptr +
          (self->len - n) *
          self->esize);

 err_overflow:
  errno = EOVERFLOW;
  return (NULL);
}

void vec_rev(Vec *const self, const size_t n) {
  assert(self != NULL);
  assert(self->len >= n);
  self->len -= n;
}

void vec_iter(Vec *const self, VecCB iter) {
  assert(self != NULL);
  assert(iter != NULL);

  uint8_t *p = self->ptr;

  for (size_t i = 0; i < self->len; i++, p += self->esize)
    iter(p);
}
