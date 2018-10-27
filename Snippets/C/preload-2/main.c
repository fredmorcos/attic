#include <stdio.h>

#include "lib.h"

void leave (int *x) {
  printf("byebye %d\n", *x);
}

int main (void) {
  __attribute__((cleanup(leave))) int x = foo();
  return 0;
  printf("%d\n", x);
  return 0;
}
