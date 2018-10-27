#include "array.h"

Array(char)

int main (void) {
  struct char_array arr;

  char_array_init(&arr, 0, 0);
  char_array_free(&arr);

  return 0;
}
