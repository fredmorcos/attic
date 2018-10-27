#pragma once
#define attr __attribute__

#include "vec.h"

int lsdir_parse(char **const buf, Vec *const res)
  attr((warn_unused_result, nonnull));
