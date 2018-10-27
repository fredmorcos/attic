#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include "util.h"
#include "char-buffer.h"

#ifdef DEBUG
#include <err.h>
#include <sys/time.h>
#include <time.h>
#endif

void char_buffer_init(struct char_buffer *const self, const size_t chunk) {
  assert(self);

  (void) memset(self, 0, sizeof(struct char_buffer));

  self->ptr = NULL;
  self->len = 0;
  self->cap = 0;
  self->chunk = chunk;
}

void char_buffer_free(struct char_buffer *const self) {
  assert(self);

#ifdef DEBUG
  warnx("DEBUG: %s:%d", __FILE__, __LINE__);
  warnx(" Free (length/capacity): %p", (void *) self->ptr);
  warnx("   Final: %zu/%zu", self->len, self->cap);
#endif

  if (!self->ptr) {
    assert(self->len == 0);
    assert(self->cap == 0);
    return;
  }

  free(self->ptr);
}

void char_buffer_revn(struct char_buffer *const self, const size_t n) {
  assert(self);
  assert(self->len >= n);
  self->len -= n;
}

bool char_buffer_extn(struct char_buffer *const self, const size_t n) {
  return char_buffer_addn(self, n) == NULL ? false : true;
}

char *char_buffer_addn(struct char_buffer *const self, const size_t n) {
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

    if (!(new_ptr = xrealloc(self->ptr, new_cap, sizeof(char))))
      return NULL;

#ifdef DEBUG
    tidiff(&ti);
    warnx("DEBUG: %s:%d", __FILE__, __LINE__);
    warnx(" %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc", ti.cpu, ti.rt, ti.wc);
    warnx(" Realloc (length/capacity): %p → %p", (void *) self->ptr, new_ptr);
    warnx("  Need: %zu elements", n);
    warnx("  Old:  %zu/%zu", self->len, self->cap);
    warnx("  New:  %zu/%zu", self->len + n, new_cap);
#endif

    self->ptr = new_ptr;
    self->cap = new_cap;
  }

  self->len += n;

  return self->ptr + (self->len - n) * sizeof(char);

 err_overflow:
  errno = EOVERFLOW;
  return NULL;
}
