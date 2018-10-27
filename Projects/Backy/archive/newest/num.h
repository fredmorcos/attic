#pragma once

#include <stdbool.h>

unsigned int ndigits(unsigned long long val)
  __attribute__((warn_unused_result, const));

void ulltoa(char *str, unsigned long long val, const unsigned int len)
  __attribute__((nonnull));

bool strtonum(const char *const numstr,
              long long minval,
              long long maxval,
              long long *const res)
  __attribute__((warn_unused_result, nonnull));
