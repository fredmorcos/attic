#include <stdio.h>

struct foo {
  long b;
  long c[];
};

struct bar {
  long b;
  long c[0];
  long d;
};

struct baz {
  long b;
  long c[2];
};

struct primitive_struct {
    int field1;
    char field2;
    long field3;
};

void test(struct foo *f,   struct bar *b,  struct baz *z) {
  printf("f = %zu\n", f->c[1]);
  printf("b = %zu\n", b->c[2]);
  printf("z = %zu\n", z->c[3]);
}

int main() {
  struct foo f;
  struct bar b;
  struct baz z;

  printf("sizeof(f) = %zu\n", sizeof(f));
  printf("sizeof(b) = %zu\n", sizeof(b));
  printf("sizeof(z) = %zu\n", sizeof(z));

  printf("sizeof(primitive_struct) = %zu\n", sizeof(struct primitive_struct));
}
