#include "buffer.h"
#include <string.h>
#include <assert.h>
#include <talloc.h>

struct buffer *
buffer_init(void *ctx, size_t initial_len, size_t element_size) {
  assert(initial_len > 0);
  assert(element_size > 0);

  struct buffer *buffer = NULL;

  if (!(buffer = talloc_zero(NULL, struct buffer))) {
    return NULL;
  }

  if (!(buffer->ptr = talloc_size(buffer, initial_len * element_size))) {
    TALLOC_FREE(buffer);
    return NULL;
  }

  buffer->allocated_len = initial_len;
  buffer->element_size = element_size;
  talloc_reparent(NULL, ctx, buffer);

  return buffer;
}

size_t
buffer_append(struct buffer *buffer, void *element) {
  assert(buffer != NULL);
  assert(element != NULL);

  void *new_ptr = NULL;
  char *buf_ptr = NULL;

  if (buffer->len == buffer->allocated_len) {
    if (!(new_ptr = talloc_realloc_size
          (buffer,
           buffer->ptr,
           buffer->element_size * buffer->allocated_len * 2))) {
      return -1;
    } else {
      buffer->ptr = new_ptr;
      buffer->allocated_len *= 2;
    }
  }

  buf_ptr = buffer->ptr;
  memcpy(buf_ptr + (buffer->len * buffer->element_size / sizeof(char)),
         element,
         buffer->element_size);
  buffer->len++;

  return buffer->len - 1;
}

struct buffer *
buffer_reduce(struct buffer *buffer) {
  assert(buffer != NULL);
  assert(buffer->len > 0);

  void *new_ptr = NULL;

  if (buffer->len < buffer->allocated_len) {
    if (!(new_ptr = talloc_realloc_size
          (buffer,
           buffer->ptr,
           buffer->element_size * buffer->len))) {
      return NULL;
    } else {
      buffer->ptr = new_ptr;
      buffer->allocated_len = buffer->len;
    }
  }

  return buffer;
}

__attribute__((always_inline)) inline void *
buffer_index(struct buffer *buffer, size_t index) {
  assert(buffer);
  assert(buffer->ptr);

  return index >= buffer->len ? NULL :
    (void *) ((char *) buffer->ptr + index * buffer->element_size);
}
