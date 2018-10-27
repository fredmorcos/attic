#include "extra.h"

int iselem (int c, char const *l) {
  for (; *l != '\0'; l++) {
    if (*l == c) return 1;
  }

  return 0;
}
