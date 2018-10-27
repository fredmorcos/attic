#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <err.h>
#include "util.h"
#include "vec.h"

#ifdef DEBUG
#include <sys/time.h>
#include <time.h>
#endif

void vec_init(struct vec *const self,
              const size_t chunk,
              const size_t esize,
              vec_cb *const free_cb) {
  assert(self);

  (void) memset(self, 0, sizeof(struct vec));

  self->ptr = NULL;
  self->len = 0;
  self->cap = 0;
  self->chunk = chunk;
  self->esize = esize;
  self->free_cb = free_cb;
}

void vec_free(struct vec *const self) {
  assert(self);

#ifdef DEBUG
  warnx("DEBUG: %s:%d", __FILE__, __LINE__);
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

void vec_revn(struct vec *const self, const size_t n) {
  assert(self);
  assert(self->len >= n);
  self->len -= n;
}

bool vec_extn(struct vec *const self, const size_t n) {
  return vec_addn(self, n) == NULL ? false : true;
}

void *vec_addn(struct vec *const self, const size_t n) {
  assert(self);
  assert(n > 0);
  assert(self->len <= self->cap);

  if (self->len + n > self->cap) {
    void *new_ptr = NULL;
    size_t new_cap = 0;

    if (n > self->cap) {
      if (self->cap > SIZE_MAX - n)
        goto err_overflow;

      new_cap = self->cap + n;
    } else {
      if (self->cap > SIZE_MAX / 2)
        goto err_overflow;

      new_cap = self->cap * 2;
    }

    new_cap = new_cap > self->chunk ? new_cap : self->chunk;

#ifdef DEBUG
    struct time_info ti;
    tinow(&ti, true);
#endif

    if (!(new_ptr = xrealloc(self->ptr, new_cap, self->esize)))
      return NULL;

#ifdef DEBUG
    tidiff(&ti);
    warnx("DEBUG: %s:%d", __FILE__, __LINE__);
    warnx(" %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc", ti.cpu, ti.rt, ti.wc);
    warnx(" Reallocation (length/capacity): %p → %p", self->ptr, new_ptr);
    warnx("  Need: %zu elements", n);
    warnx("  Old:  %zu/%zu", self->len, self->cap);
    warnx("  New:  %zu/%zu", self->len + n, new_cap);
#endif

    self->ptr = new_ptr;
    self->cap = new_cap;
  }

  self->len += n;

  return (uint8_t *) self->ptr + (self->len - n) * self->esize;

 err_overflow:
  errno = EOVERFLOW;
  return NULL;
}
