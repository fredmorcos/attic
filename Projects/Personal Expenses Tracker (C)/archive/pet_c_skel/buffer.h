#ifndef PET_BUFFER
#define PET_BUFFER

#include <stdlib.h>
#include <stdbool.h>

struct buffer_t {
  void * buf;
  size_t len;
};

struct buffer_t * buffer_new (const size_t, const size_t);
void buffer_destroy (struct buffer_t *);
bool buffer_expand (struct buffer_t *,
                    const size_t,
                    const size_t);

#endif
