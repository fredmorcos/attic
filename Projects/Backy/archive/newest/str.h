#pragma once

#include <stdbool.h>
#include <stdlib.h>

size_t xstrlcpy(char *dst, const char *src, const size_t dsize)
  __attribute__((warn_unused_result, nonnull));

int xasprintf(size_t *const l, char **const s, const char *const f, ...)
  __attribute__((warn_unused_result, format(printf, 3, 4), nonnull(2, 3)));

char *humansize(long double size)
  __attribute__((warn_unused_result));

bool strchars(const char *const buf,
              const size_t len,
              const char ch,
              const size_t num)
  __attribute__((warn_unused_result, nonnull, pure));
