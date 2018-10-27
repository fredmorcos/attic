#ifndef CTX_MM_H

#include <stdlib.h>

struct ctx_mm_t {
  void   ** free_l;
  size_t    len;

  /* private fields */
  size_t    real_len_;
};

void   ctx_mm_init  (struct ctx_mm_t * const);
void * ctx_malloc   (const size_t, struct ctx_mm_t * const);
void   ctx_free     (void * const, struct ctx_mm_t * const);
void   ctx_free_all (struct ctx_mm_t * const);

#endif  /* CTX_MM_H */
