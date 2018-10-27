#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include "mem.h"

void *xrealloc(void **const p,
               const size_t n,
               const size_t size) {
  assert(p != NULL);
  assert(n != 0);

  /* Overflow check from OpenBSD: This is sqrt(SIZE_MAX
   * + 1),  since (s1 * s2)  is <= SIZE_MAX if  both s1
   * and s2 are < MUL_NO_OFLOW.
   */
  #define MUL_NO_OFLOW \
    ((size_t) 1 << (sizeof(size_t) * 4))

  void *new_p = NULL;

  bool overflow =
    (n >= MUL_NO_OFLOW ||
     size >= MUL_NO_OFLOW) &&
    n > 0 && SIZE_MAX / n < size;

  if (overflow) {
    errno = EOVERFLOW;
    return (NULL);
  }

  new_p = realloc(*p, n * size);

  if (new_p == NULL)
    return (NULL);

  return (*p = new_p);
}

void str_free(char **const p) {
  assert(p != NULL);

  if (*p != NULL)
    free(*p);
}
