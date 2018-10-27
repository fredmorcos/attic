#pragma once

#include <stdlib.h>

void *xrealloc(void **const p, const size_t n, const size_t size)
  __attribute__((warn_unused_result, malloc, alloc_size(2, 3), nonnull));

void char_free(char **const p)
  __attribute__((nonnull));
