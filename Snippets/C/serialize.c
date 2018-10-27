/*
 * clang -Wall -Wextra -pedantic -std=c11 serialize.c -o serialize -g
 */

#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int a;
  double b;
} bar;

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  FILE *f;
  bar b;

  if ((f = fopen("serialize.data", "rb")) == NULL) {
    b.a = 5;
    b.b = 3.4;

    f = fopen("serialize.data", "wb");
    fwrite(&b, 1, sizeof(b), f);
    fclose(f);
  } else {
    fread(&b, 1, sizeof(b), f);
    fclose(f);
    printf("%d\n%f\n", b.a, b.b);
  }

  return 0;
}
