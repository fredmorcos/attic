#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include "mem.h"
#include "svec.h"

SVec svec_init(const size_t esize,
               const size_t chunk,
               SVecCB dtor) {
  assert(esize > 0);
  assert(chunk > 0);

  SVec self;

  (void) memset(&self, 0, sizeof(SVec));

  self.ptr = NULL;
  self.len = 0;
  self.chunk = chunk;
  self.esize = esize;
  self.dtor = dtor;

  return (self);
}

void svec_free(SVec *const self) {
  assert(self != NULL);

  if (self->dtor != NULL)
    svec_iter(self, self->dtor);

  if (self->ptr == NULL) {
    assert(self->len == 0);
  } else {
    const size_t chunks = self->len / self->chunk;

    for (size_t i = 0; i < chunks; i++) {
      uint8_t *p = self->ptr[i];
      free(p);
    }

    free(self->ptr);

    self->ptr = NULL;
    self->len = 0;
    self->dtor = NULL;
  }
}

void *svec_add(SVec *const self) {
  assert(self != NULL);

  const size_t chunks = self->len / self->chunk;
  void **new_ptr = NULL;

  if (self->len == SIZE_MAX) {
    errno = EOVERFLOW;
    return (NULL);
  }

  if (self->len % self->chunk == 0) {
    void *chunk_ptr = NULL;

    new_ptr = xrealloc((void **) &self->ptr,
                       chunks + 1,
                       sizeof(void **));

    if (new_ptr == NULL)
      return (NULL);

    self->ptr = new_ptr;

    chunk_ptr = xrealloc(&chunk_ptr,
                         self->chunk,
                         self->esize);

    if (chunk_ptr == NULL)
      return (NULL);

    self->ptr[chunks] = chunk_ptr;
  }

  self->len++;

  return ((uint8_t *) self->ptr[chunks] +
          (self->len -
           chunks * self->chunk) *
          self->esize);
}

void svec_iter(SVec *const self, SVecCB iter) {
  assert(self != NULL);
  assert(iter != NULL);

  const size_t chunks = self->len / self->chunk;

  for (size_t i = 0; i < chunks; i++) {
    uint8_t *p = self->ptr[i];

    for (size_t j = 0; j < self->chunk; j++) {
      iter(p);
      p += self->esize;
    }
  }
}
