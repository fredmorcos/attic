#include <assert.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include "vec.h"
#include "mem.h"

inline void vec_init(struct vec *const self,
#if defined(NDEBUG)
                     __attribute__((unused))
#endif
                     const char *const name,
                     const size_t esize,
                     const size_t chunk,
                     vec_cb dtor) {
  assert(self != NULL);
  assert(esize > 0);
  assert(chunk > 0);

  (void) memset(self, 0, sizeof(struct vec));

  self->ptr = NULL;
  self->len = 0;
  self->cap = 0;
  self->chunk = chunk;
  self->esize = esize;
  self->dtor = dtor;

#if !defined(NDEBUG)
  {
    assert(name != NULL);
    self->name = name;
    self->nreallocs = 0;
  }
#endif
}

void vec_free(struct vec *const self) {
  assert(self != NULL);

#if !defined(NDEBUG)
  {
    assert(self->name != NULL);
    warnx("DEBUG vec_free(): %s (%zu reallocs, %zu/%zu elems)",
          self->name, self->nreallocs, self->len, self->cap);
  }
#endif

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

void *vec_add(struct vec *const self, const size_t n) {
  assert(self != NULL);
  assert(self->len <= self->cap);

  void *new_ptr = NULL;

  if (SIZE_MAX - n < self->len) {
    goto err_oflow;
  } else if (self->len + n > self->cap) {
    if (SIZE_MAX / 2 < self->cap) {
      goto err_oflow;
    } else {
      size_t new_cap = 0;

      new_cap = (self->len + n) > (self->cap * 2) ?
        (self->len + n) : (self->cap * 2);
      new_cap = new_cap > self->chunk ? new_cap : self->chunk;

      assert(new_cap > self->cap);
      assert(new_cap >= self->len + n);

      if (!(new_ptr = xrealloc(&self->ptr, new_cap, self->esize)))
        return (NULL);

      self->ptr = new_ptr;
      self->cap = new_cap;

#if !defined(NDEBUG)
      {
        self->nreallocs++;
      }
#endif
    }
  }

  self->len += n;

  return ((uint8_t *) self->ptr + (self->len - n) * self->esize);

 err_oflow:
  errno = EOVERFLOW;
  return (NULL);
}

void vec_rev(struct vec *const self, const size_t n) {
  assert(self != NULL);
  assert(self->len >= n);
  self->len -= n;
}

inline void vec_iter(struct vec *const self, vec_cb iter) {
  assert(self != NULL);
  assert(iter != NULL);

  uint8_t *p = self->ptr;

  for (size_t i = 0; i < self->len; i++, p += self->esize)
    iter(p);
}
