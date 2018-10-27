#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include "mem.h"

void *xrealloc(void **const p, const size_t n, const size_t size) {
  /* OpenBSD:  This is  sqrt(SIZE_MAX  + 1),  since (s1  *  s2) is  <=
   * SIZE_MAX if both s1 and s2 are < MUL_NO_OFLOW.
   */
#define MUL_NO_OFLOW ((size_t) 1 << (sizeof(size_t) * 4))

  assert(p != NULL);

  /* XXX Fix these in the overflow check below */
  assert(n != 0);
  assert(size != 0);

  void *new_p = NULL;

  if ((n >= MUL_NO_OFLOW || size >= MUL_NO_OFLOW) &&
      n > 0 && SIZE_MAX / n < size) {
    errno = EOVERFLOW;
    return (NULL);
  }

  if ((new_p = realloc(*p, n * size)) == NULL)
    return (NULL);

  return (*p = new_p);
}

inline void char_free(char **const p) {
  assert(p != NULL);

  if (*p != NULL)
    free(*p);
}
