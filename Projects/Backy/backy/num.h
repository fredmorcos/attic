#pragma once
#define attr __attribute__

#include <stdbool.h>

typedef unsigned long long ull;
typedef unsigned int       uint;

unsigned int ndigits(ull val)
  attr((warn_unused_result, const));

void ulltoa(char *str, ull val, const uint len)
  attr((nonnull));

bool strtonum(const char *const numstr,
              long long minval,
              long long maxval,
              long long *const res)
  attr((warn_unused_result, nonnull));
