#pragma once

#include "vec.h"

int lsdir_parse(char **const buf, struct vec *const res)
  __attribute__((warn_unused_result, nonnull));
