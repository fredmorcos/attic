#include "extra.h"
#include <stdio.h>
#include <stdlib.h>

void
extra_die(const char *msg, ...)
{
  va_list args;

  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  exit(EXIT_FAILURE);
}
