#include "buffer.h"
#include <string.h>
#include <assert.h>
#include <talloc.h>

struct buffer *buffer_new(int initial_len, size_t element_size) {
  struct buffer *buf = NULL;

  assert(initial_len > 0);
  assert(element_size > 0);

  if ((buf = malloc(sizeof(struct buffer))) == NULL) {
    return NULL;
  }

  if ((buf->ptr = malloc(initial_len * element_size)) == NULL) {
    free(buf);
    return NULL;
  }

  buf->allocated_len = initial_len;
  buf->element_size = element_size;
  buf->len = 0;

  return buf;
}

struct buffer *buffer_append(struct buffer *buf, void *element) {
  void *new_ptr = NULL;
  char *buf_ptr = NULL;

  assert(buf != NULL);
  assert(buf->ptr != NULL);
  assert(element != NULL);

  if (buf->len == buf->allocated_len) {
    if ((new_ptr = realloc
         (buf->ptr, buf->allocated_len * buf->element_size * 2)) == NULL) {
      return NULL;
    } else {
      buf->ptr = new_ptr;
      buf->allocated_len *= 2;
    }
  }

  buf_ptr = buf->ptr;
  memcpy(buf_ptr + (buf->len * buf->element_size / sizeof(char)),
         element, buf->element_size);
  buf->len++;

  return buf;
}
