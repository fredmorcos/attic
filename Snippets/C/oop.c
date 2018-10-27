#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int i;
} Foo;

typedef struct {
  Foo super;
  char *j;
} Bar;

void print_foo (Foo *f) {
  printf("%d\n", f->i);
}

int main (void) {
  Bar b = { .super = { .i = 5 }, .j = "Hello" };
  Bar *b2 = (Bar *) malloc (sizeof(Bar));

  ((Foo *) b2)->i = 6;
  b2->j = "World";

  print_foo ((Foo *) &b);
  print_foo ((Foo *) b2);

  free(b2);
  return 0;
}
