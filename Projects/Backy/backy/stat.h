#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <time.h>
#include <sys/stat.h>

void xfinfo(const struct stat *const st,
            time_t *const mtime,
            long long *const size)
  attr((nonnull));

bool xfstat(const char *const path,
            time_t *const mtime,
            long long *const size)
  attr((warn_unused_result, nonnull));

bool xlstatlen(const char *const path,
               size_t *const tlen)
  attr((warn_unused_result, nonnull));

bool xlstat(const char *const path,
            char *const tg,
            const size_t tlen)
  attr((warn_unused_result, nonnull));
