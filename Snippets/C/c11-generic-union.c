/*
 * gcc -Wall -Wextra -pedantic -std=c11 c11_generic_union.c -o c11_generic_union
 */

#include <stdio.h>

enum val_type {
  val_type_int,
  val_type_double,
  val_type_str,
  val_type_char
};

struct val {
  enum val_type type;

  union {
    int i;
    double d;
    char *s;
    char c;
  };
};

void print(struct val *v) {
  switch(v->type) {
  case val_type_int:
    printf("%d", v->i);
    break;
  case val_type_double:
    printf("%f", v->d);
    break;
  case val_type_char:
    printf("%c", v->c);
    break;
  case val_type_str:
    printf("%s", v->s);
    break;
  }
}

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  print(&((struct val) { .type = val_type_int    , .i = 5                }));
  print(&((struct val) { .type = val_type_char   , .c = '\n'             }));
  print(&((struct val) { .type = val_type_double , .d = 5.5              }));
  print(&((struct val) { .type = val_type_char   , .c = '\n'             }));
  print(&((struct val) { .type = val_type_str    , .s = (char *) "hello" }));
  print(&((struct val) { .type = val_type_char   , .c = '\n'             }));

  return 0;
}
