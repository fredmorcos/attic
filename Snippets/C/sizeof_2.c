#include <stdlib.h>
#include <stdio.h>

struct A {
  int a; // 4
  struct __attribute__((packed)) {
    char a; // 1
    char b; // 1
    int c; // 4
    short d; // 2
  } s;
  int b; // 4
};

struct __attribute__((packed)) A2 {
  int a; // 4
  struct {
    char a; // 1
    char b; // 1
    int c; // 4
    short d; // 2
  } s;
  int b; // 4
};

int main () {
  printf("%zu\n", sizeof(struct A));
  printf("%zu\n", sizeof(struct A2));
}
