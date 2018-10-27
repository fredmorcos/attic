#include <stdio.h>
#include <stdlib.h>
#include "config.h"

int main (int argc, char **argv)
{
  printf("You provided %d arguments\n", argc);
  printf("Foo's value is %d\n", FOO);
  return 0;
}
