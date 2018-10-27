#include <stdio.h>
#include <stdlib.h>

void foo(char *c) {
  printf("fooooo %c\n", *c);
}

void foz(char *c) {
  printf("foz %c\n", *c);
}

int main (void) {
  __attribute__((cleanup(foo))) char bar;
  __attribute__((cleanup(foz))) char baz;

  bar = '4';
  baz = '5';

  return 1;

  printf("%c\n", bar);
  printf("%c\n", baz);

  return 0;
}
