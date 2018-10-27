#pragma once

#include <stdbool.h>
#include <dirent.h>

char *xrealpath(const char *const path)
  __attribute__((warn_unused_result, nonnull));

DIR *xopendir(const char *const name)
  __attribute__((warn_unused_result, nonnull));

void xclosedir(DIR **const dir)
  __attribute__((nonnull));

bool xchdir(DIR *const dir, const char *const name)
  __attribute__((warn_unused_result, nonnull));
