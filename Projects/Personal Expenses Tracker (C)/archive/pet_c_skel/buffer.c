#include "buffer.h"
#include "log.h"
#include <stdlib.h>
#include <stdbool.h>

struct buffer_t * buffer_new (const size_t esize, const size_t len) {
  struct buffer_t * buffer = malloc(sizeof(struct buffer_t));

  if (buffer != NULL) {
    buffer->buf = malloc(esize * len);

    if (buffer->buf == NULL) {
      free(buffer);
      buffer = NULL;
    } else {
      buffer->len = len;
    }
  }

  return buffer;
}

void buffer_destroy (struct buffer_t * buffer) {
  free(buffer->buf);
  free(buffer);
}

bool buffer_expand (struct buffer_t * buffer,
                    const size_t esize,
                    const size_t inc) {
  void * newbuf = realloc(buffer->buf, esize * (buffer->len + inc));

  if (newbuf == NULL) {
    return false;
  }

  buffer->buf = newbuf;
  buffer->len += inc;

  return true;
}
