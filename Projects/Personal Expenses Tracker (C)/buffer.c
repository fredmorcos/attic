#include "buffer.h"
#include <string.h>
#include <assert.h>
#include <talloc.h>

buf_t *buf_init (void *ctx, int ilen, size_t esize) {
  buf_t *buf = NULL;

  assert(ilen > 0);
  assert(esize > 0);

  if (!(buf = talloc_zero(NULL, buf_t))) {
    return NULL;
  }

  if (!(buf->ptr = talloc_zero_size(buf, ilen * esize))) {
    TALLOC_FREE(buf);
    return NULL;
  }

  buf->ctx = ctx;
  buf->alen = ilen;
  buf->esize = esize;
  talloc_reparent(NULL, ctx, buf);

  return buf;
}

buf_t *buf_append (buf_t *buf, void *elem) {
  void *new_ptr = NULL;
  char *buf_ptr = NULL;

  assert(buf != NULL);
  assert(elem != NULL);

  if (buf->len == buf->alen) {
    if (!(new_ptr = talloc_realloc_size
          (buf->ctx, buf->ptr, buf->esize * buf->alen * 2))) {
      return NULL;
    } else {
      buf->ptr = new_ptr;
      buf->alen *= 2;
    }
  }

  buf_ptr = buf->ptr;
  memcpy(buf_ptr + (buf->len * buf->esize / sizeof(char)), elem, buf->esize);
  buf->len++;

  return buf;
}
