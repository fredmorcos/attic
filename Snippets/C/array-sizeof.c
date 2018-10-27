#include <stdio.h>

void bar(int foo[static 3]) {
  int *x = foo;
  printf("size of foo from bar: %lu\n", sizeof(x));
}

int main() {
  int foo[] = {1, 2, 3, 4};
  int *x = foo;
  int (*y)[4] = &foo;

  printf("size of foo: %lu\n", sizeof(foo));

  bar(foo);

  return 0;
}
