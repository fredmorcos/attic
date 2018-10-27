#pragma once

#include <stdlib.h>
#include "obj.h"

struct str {
  struct obj parent;
  size_t len;
  char *cstr;
};

int str_init(struct str *const str,
             const bool autofree,
             const char *const cstr);
int str_set(struct str *const str,
            const char *const cstr);
