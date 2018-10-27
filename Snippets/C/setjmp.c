#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>

static char * foo = NULL;

void alloc_err_handler(jmp_buf * b) {
  if (setjmp(*b)) {
    printf("handle error\n");
    free(foo);
  }
}

void alloc_err(jmp_buf * b) {
  foo = malloc(sizeof(char) * 2);
  printf("allocation error!\n");
  longjmp(*b, 1);
  printf("finished handling error\n");
}

void second(jmp_buf * b) {
  printf("second\n");
  longjmp(*b, 1);
}

void first(jmp_buf * b) {
  second(b);
  printf("first\n");
}

int main() {
  static jmp_buf buf;

  if (!setjmp(buf)) {
    first(&buf);
  } else {
    printf("main\n");
  }

  alloc_err_handler(&buf);

  alloc_err(&buf);

  printf("finishing\n");

  return 0;
}
