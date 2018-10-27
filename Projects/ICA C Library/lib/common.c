#include "common.h"

#include <stdio.h>
#include <stdlib.h>

void error_exit(const char *msg)
{
  printf("Error: %s\n", msg);
  exit(1);
}
