#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef void (arr_cb)(void *);
typedef arr_cb ArrCB;

struct arr {
  void   *ptr;                          /* actual array */
  size_t  len;                          /* used length */
  size_t  alen;                         /* allocated length */
  size_t  ilen;                         /* initial allocated length */
  size_t  esize;                        /* element size in bytes */
  ArrCB  *free_cb;                      /* element free callback */
};

typedef struct arr Arr;

#define w_unused_result __attribute__((warn_unused_result))
#define malloc_like     __attribute__((malloc))

#define AutoArr __attribute__((cleanup(arr_free))) struct arr

void  arr_init(Arr *const a, const size_t ilen, const size_t esize, ArrCB *cb);
void  arr_free(Arr *const a);
void  arr_delete(Arr *const a, const size_t i);
void  arr_revert(Arr *const a, const size_t n);
int   arr_extend(Arr *const a, const size_t n) w_unused_result;
void *arr_add(Arr *const a, const size_t n, const bool zero)
  w_unused_result malloc_like;
