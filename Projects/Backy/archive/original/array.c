#include <assert.h>
#include <stdbool.h>
#include <bsd/bsd.h>
#include "array.h"

void
array_init(struct array *const a,
           const size_t esize,
           array_cb cb)
{
  assert(esize > 0);
  (void) memset(a, 0, sizeof(struct array));
  a->esize = esize;
  a->free_cb = cb;
}

void
array_revertn(struct array *const a,
              const size_t n)
{
  assert(a->len >= n);
  a->len -= n;
}

bool
array_addn(struct array *const a,
           const size_t n,
           const bool zero,
           void **const res)
{
  assert(a->len <= a->alen);

  if (n != 0 && a->len + n > a->alen) {
    const size_t _init_alen = 2048;

    void *new_ptr = NULL;
    size_t new_alen = 0;

    new_alen = n > a->alen ? a->alen + n : a->alen * 2;
    new_alen = new_alen > _init_alen ? new_alen : _init_alen;

    if (!(new_ptr = reallocarray(a->ptr, new_alen, a->esize))) {
      warn("reallocarray(): Could not expand array");

      if (res) {
        *res = NULL;
      }

      return false;
    }

    a->ptr = new_ptr;
    a->alen = new_alen;
  }

  if (zero) {
    (void) memset((char *) a->ptr + a->len * a->esize, 0, n * a->esize);
  }

  a->len += n;

  if (res) {
    *res = (char *) a->ptr + (a->len - n) * a->esize;
  }

  return true;
}

void
array_traverse(const struct array *const a,
               void (*traverse_cb)(void *))
{
  assert(traverse_cb);

  char *p = a->ptr;

  for (size_t i = 0; i < a->len; i++, p += a->esize) {
    traverse_cb(p);
  }
}

void
array_traverse1(const struct array *const a,
                void (*traverse_cb1)(void *, void *),
                void *const arg1)
{
  assert(traverse_cb1);

  char *p = a->ptr;

  for (size_t i = 0; i < a->len; i++, p += a->esize) {
    traverse_cb1(p, arg1);
  }
}

void
array_free(struct array *const a)
{
  if (a->free_cb) {
    for (size_t i = 0; i < a->len; i++) {
      a->free_cb((char *) a->ptr + i * a->esize);
    }
  }

  if (a->alen == 0) {
    assert(a->ptr == NULL);
  } else {
    assert(a->ptr);
    free(a->ptr);
  }
}
