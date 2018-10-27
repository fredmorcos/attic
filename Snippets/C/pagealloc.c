#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

int main (void) {
  void *p = NULL;
  void *pnew = NULL;
  size_t s = 1024;

  for (size_t i = 0; i < 100; i++) {
    pnew = realloc(p, s);

    if (!pnew) {
      perror("realloc");
      free(p);
      return 0;
    }

    p = pnew;

    printf("%zu: p = %p  s = %zu\n", i, p, s);

    s *= 2;
  }

  free(p);

  return 0;
}
