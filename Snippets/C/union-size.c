#include <stdio.h>

typedef union _bar {
  int c;
  double d;
} bar;

typedef struct _foo {
  int a;
  int b;
  bar c;
  int e;
} foo;

int main (int argc, char **argv)
{
  printf("sizeof(int)    = %d\n", sizeof(int));
  printf("sizeof(double) = %d\n", sizeof(long));
  printf("sizeof(bar)    = %d\n", sizeof(bar));
  printf("sizeof(foo)    = %d\n", sizeof(foo));

  bar b1 = { .c = 2 };
  bar b2 = { .d = 4.0 };

  printf("sizeof(b1) = %d\n", sizeof(b1));
  printf("sizeof(b2) = %d\n", sizeof(b2));

  foo f1 = { .a = 1, .b = 2, .c = b1, .e = 1 };
  foo f2 = { .a = 1, .b = 2, .c = b2, .e = 1 };

  printf("sizeof(f1) = %d\n", sizeof(f1));
  printf("sizeof(f2) = %d\n", sizeof(f2));

  return 0;
}
