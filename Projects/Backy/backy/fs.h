#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <dirent.h>

char *xrealpath(const char *const path)
  attr((warn_unused_result, nonnull));

DIR *xopendir(const char *const name)
  attr((warn_unused_result, nonnull));

void xclosedir(DIR **const dir)
  attr((nonnull));

bool xchdir(DIR *const dir, const char *const name)
  attr((warn_unused_result, nonnull));
