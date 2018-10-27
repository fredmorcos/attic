#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <stdlib.h>

size_t xstrlcpy(char *dst,
                const char *src,
                const size_t dsize)
  attr((warn_unused_result, nonnull));

int xasprintf(size_t *const l,
              char **const s,
              const char *const f, ...)
  attr((warn_unused_result,
        format(printf, 3, 4),
        nonnull(2, 3)));

char *humansize(long double size)
  attr((warn_unused_result));

bool strchars(const char *const buf,
              const size_t len,
              const char ch,
              const size_t num)
  attr((warn_unused_result, nonnull, pure));
