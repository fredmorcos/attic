#include <bsd/stdlib.h>

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "arr.h"

void arr_init(Arr *const a, const size_t ilen, const size_t esize, ArrCB *cb) {
  assert(esize > 0);

  (void) memset(a, 0, sizeof(Arr));

  a->ilen    = ilen;
  a->esize   = esize;
  a->free_cb = cb;
}

void arr_free(Arr *const a) {
  if (a->alen == 0) {
    assert(a->len == 0);
    assert(a->ptr == NULL);
  } else {
    uint8_t *const ptr = a->ptr;

    if (a->free_cb)
      for (size_t i = 0; i < a->len; i++)
        a->free_cb(ptr + i * a->esize);

    assert(a->ptr);
    free(a->ptr);
  }
}

void arr_delete(Arr *const a, const size_t i) {
  assert(i < a->len);

  uint8_t *ptr = a->ptr;

  if (i < a->len - 1)
    (void) memcpy(ptr + i * a->esize, ptr + (a->len - 1) * a->esize, a->esize);

  a->len--;
}

void arr_revert(Arr *const a, const size_t n) {
  assert(a->len >= n);
  a->len -= n;
}

int arr_extend(Arr *const a, const size_t n) {
  return !arr_add(a, n, false) ? errno : 0;
}

void *arr_add(Arr *const a, const size_t n, const bool zero) {
  assert(a->len <= a->alen);

  if (n != 0 && a->len + n > a->alen) {
    uint8_t *new_ptr  = NULL;
    size_t   new_alen = 0;

    new_alen = n > a->alen ? a->alen + n : a->alen * 2;
    new_alen = new_alen > a->ilen ? new_alen : a->ilen;

    if (!(new_ptr = reallocarray(a->ptr, new_alen, a->esize)))
      return NULL;

    a->ptr  = new_ptr;
    a->alen = new_alen;
  }

  {
    uint8_t *ptr = a->ptr;

    if (zero)
      (void) memset(ptr + a->len * a->esize, 0, n * a->esize);

    a->len += n;

    return ptr + (a->len - n) * a->esize;
  }
}
