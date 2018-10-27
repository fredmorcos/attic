#pragma once
#define attr __attribute__

#include <stdlib.h>

void *xrealloc(void **const p,
               const size_t n,
               const size_t size)
  attr((warn_unused_result,
        malloc,
        alloc_size(2, 3),
        nonnull));

#define AutoStr \
  attr((cleanup(str_free))) char *

void str_free(char **const p)
  attr((nonnull));
