#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INT __attribute__((cleanup(intclean))) int

void func(const char *const str);
void intclean(const int *const i);
void intsomething(INT z);

void func(const char *const str) {
  printf("%s\n", str);
}

void intclean(const int *const i) {
  printf("intclean: %d\n", *i);
}

void intsomething(INT z) {
  printf("intsomething: %d\n", z);
}

int main (void) {
  int x = 5;
  void *p = &x;
  int y = *((int *) p);

  printf("%d\n", y);

  char *s = strdup("hello");
  func(s);
  free(s);

  INT z = 5;
  intsomething(z);

  return 0;
}
