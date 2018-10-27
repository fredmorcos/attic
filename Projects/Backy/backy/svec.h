#pragma once
#define attr __attribute__

#include <stdlib.h>

typedef void (*SVecCB)(void *const);

struct svec {
  void   **ptr;
  size_t   len;
  size_t   chunk;
  size_t   esize;
  SVecCB   dtor;
} attr((designated_init));

typedef struct svec SVec;

#define AutoSVec \
  attr((cleanup(svec_free))) SVec

SVec svec_init(const size_t esize,
               const size_t chunk,
               SVecCB dtor);

void svec_free(SVec *const self)
  attr((nonnull));

void *svec_add(SVec *const self)
  attr((malloc, warn_unused_result, nonnull));

void svec_iter(SVec *const self, SVecCB iter)
  attr((nonnull));
