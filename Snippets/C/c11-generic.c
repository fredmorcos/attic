/*
 * gcc -Wall -Wextra -pedantic -std=c11 c11_generic.c -o c11_generic
 */

#include <stdio.h>

void print_int(int x) { printf("%d", x); }
void print_double(double x) { printf("%f", x); }
void print_str(const char x[static 1]) { printf("%s", x); }
void print_char(char x) { printf("%c", x); }

#define print(x) _Generic((x),                                          \
                          int: print_int, double: print_double,         \
                          char: print_char, default: print_str)((x))

int main(int argc, __attribute__((unused)) char *argv[argc + 1]) {
  print(5);
  print((char) '\n');
  print(5.5);
  print((char) '\n');
  print("hello");
  print((char) '\n');

  return 0;
}
