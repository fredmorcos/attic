#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

struct scache {
  const char **ptr;
  size_t len;                           /* length */
  bool csen;                            /* case-sensitive */
};

const char *
scache_put(struct scache *const sc, const char *const s) {
  int (*const cmpf)
    (const char *const,
     const char *const) =
    sc->csen ? strcmp : strcasecmp;

  const char **p = sc->ptr;
  size_t i = 0;

  while (i < sc->len) {
    if (cmpf(*p, s) == 0)
      return *p;
    p++, i++;
  }

  const char **new_ptr = realloc(sc->ptr, sc->len + 1);

  if (!new_ptr)
    return NULL;

  sc->ptr = new_ptr;
  sc->ptr[sc->len] = s;
  sc->len++;

  return s;
}

void scache_free(struct scache *const sc) {
  const char **p = sc->ptr;
  size_t i = 0;

  while (i < sc->len) {
    free(p);
    p++, i++;
  }
}
