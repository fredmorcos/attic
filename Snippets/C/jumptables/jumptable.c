#include <stdio.h>
#include <stdlib.h>

typedef void (Func)(int);

/* __attribute__((always_inline)) inline void func1(void); */
/* __attribute__((always_inline)) inline void func2(void); */
void func1(int x);
void func2(int x);

/* __attribute__((always_inline)) void func1(void) { */
void func1(int x) {
  /* printf("%d\n", x); */
  x++;
  return;
}

/* __attribute__((always_inline)) void func2(void) { */
void func2(int x) {
  /* printf("%d\n", x); */
  x++;
  return;
}

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  Func *table[] = { func1, func2 };

  table[argc - 1](2);

  return 0;
}
