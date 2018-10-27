#pragma once

#include <stdbool.h>
#include <time.h>
#include <sys/stat.h>

void xfinfo(const struct stat *const st,
            time_t *const mtime,
            long long *const size)
  __attribute__((nonnull));

bool xfstat(const char *const path,
            time_t *const mtime,
            long long *const size)
  __attribute__((warn_unused_result, nonnull));

bool xlstatlen(const char *const path, size_t *const tlen)
  __attribute__((warn_unused_result, nonnull));

bool xlstat(const char *const path, char *const target, const size_t tlen)
  __attribute__((warn_unused_result, nonnull));
