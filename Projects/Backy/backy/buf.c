#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include "buf.h"

static const size_t hdr_len = offsetof(struct buf, arr);

struct buf* buf_alloc(size_t size) {
  struct buf *buf;

  if (size > SIZE_MAX - (size + 1)) {
    errno = EOVERFLOW;
    return (NULL);
  }

  buf = malloc(hdr_len + size + 1);

  if (buf != NULL) {
    /* initialize */
    buf->size = size;
    buf->used = 0;
    buf->arr[size] = '\0';
  }

  return (buf);
}
