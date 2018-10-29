#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "string.h"

int main (void) {
  AutoString str;

  string_init(&str, 10);
  char bar[] = "hello world here";
  printf("%lu %lu\n", sizeof(bar), strlen(bar));
  char *foo = string_extend(&str, sizeof(bar) - 1);
  (void) memcpy(foo, bar, sizeof(bar) - 1);
  foo = string_extend(&str, sizeof(bar) - 1);
  (void) memcpy(foo, bar, sizeof(bar) - 1);
  printf("%lu\n", (unsigned long) str.len);
  printf("%.*s\n", str.len > INT_MAX ? INT_MAX : (int) str.len, str.ptr);

  return 0;
}
