#include "string_tolower.h"
#include <ctype.h>

char *string_tolower (char *str) {
  char *str_p = str;

  while (*str_p != '\0') {
    *str_p = tolower(*str_p);
    ++str_p;
  }

  return str;
}
