#include <stdio.h>
#include <stdlib.h>

int main (int argc, __attribute__((unused)) char *argv[argc + 1]) {
  int i = 3;

  switch (i) {
  case 1:
  i_is_1:
    printf("i is 1\n");
    return 0;
    break;
  case 3:
    printf("i is 3\n");
    break;
  default:
    printf("i don't know\n");
    break;
  }

  goto i_is_1;

  return 0;
}
