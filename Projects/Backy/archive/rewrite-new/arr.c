#include <bsd/stdlib.h>

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "arr.h"

static const size_t arr_header_size = offsetof(Arr, ptr);

Arr *arr_new(const size_t ilen, const size_t esize, ArrCB *free_cb) {
  assert(ilen  > 0);
  assert(esize > 0);

  Arr *res = malloc(arr_header_size + (ilen * esize));

  if (!res)
    return NULL;

  *res = (Arr) {
    .len     = 0,
    .alen    = ilen,
    .ilen    = ilen,
    .esize   = esize,
    .free_cb = free_cb
  };

  return res;
}

void arr_free(Arr *const a) {
  if (!a)
    return;

  if (a->free_cb)
    for (size_t i = 0; i < a->len; i++)
      a->free_cb((char *) a->ptr + i * a->esize);

  free(a);
}

void arr_revn(Arr *const a, const size_t n) {
  assert(a->len >= n);
  a->len -= n;
}

int arr_extn(Arr *const a, const size_t n) {
  return !arr_addn(a, n, false) ? errno : 0;
}

void *arr_addn(Arr *const a, const size_t n, const bool zero) {
  assert(a->len <= a->alen);

  if (n != 0 && a->len + n > a->alen) {
    char   *new_ptr  = NULL;
    size_t  new_alen = 0;

    new_alen = n > a->alen ? a->alen + n : a->alen * 2;
    new_alen = new_alen > a->ilen ? new_alen : a->ilen;

    Arr *new_arr = reallocarray(a, arr_header_size + (new_alen * a->esize), 1);

    if (!new_arr)
      return NULL;

    a = new_arr;
    a->ptr  = new_ptr;
    a->alen = new_alen;
  }

  {
    char *ptr = a->ptr;

    if (zero)
      (void) memset(ptr + a->len * a->esize, 0, n * a->esize);

    a->len += n;

    return ptr + (a->len - n) * a->esize;
  }
}
