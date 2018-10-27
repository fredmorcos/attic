#include <stdio.h>
#include <stdlib.h>

__attribute__((always_inline)) inline void func1(int x);
__attribute__((always_inline)) inline void func2(int x);

__attribute__((always_inline)) void func1(int x) {
  /* printf("%d\n", x); */
  x++;
  return;
}

__attribute__((always_inline)) void func2(int x) {
  /* printf("%d\n", x); */
  x++;
  return;
}

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  static void *table[] = { &&func1_lbl, &&func2_lbl };

  goto *table[argc - 1];

 func1_lbl:
  func1(2);
  return 0;
 func2_lbl:
  func2(2);
  return 0;

  return 0;
}
