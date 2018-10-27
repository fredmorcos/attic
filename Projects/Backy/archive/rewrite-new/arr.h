#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef void (arr_cb)(void *);
typedef arr_cb ArrCB;

struct arr {
  size_t  len;                         /* used length */
  size_t  alen;                        /* allocated length */
  size_t  ilen;                        /* initial length */
  size_t  esize;                       /* element size in bytes */
  arr_cb *free_cb;                     /* element free callback */
  char    ptr[];                       /* actual array */
};

typedef struct arr Arr;

#define AutoArr __attribute__((cleanup(arr_free))) Arr

Arr  *arr_new(const size_t ilen, const size_t esize, ArrCB *free_cb);
void  arr_free(Arr *const a);
void  arr_revn(Arr *const a, const size_t n);
int   arr_extn(Arr *const a, const size_t n)
  __attribute__((warn_unused_result));
void *arr_addn(Arr *const a, const size_t n, const bool zero)
  __attribute__((warn_unused_result))
  __attribute__((malloc));
