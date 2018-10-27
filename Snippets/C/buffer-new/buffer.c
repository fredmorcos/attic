#include "buffer.h"
#include <string.h>
#include <assert.h>
#include <talloc.h>

struct buffer {
  bool    fast_prepend;
  size_t  item_size;
  size_t  alloc_len;
  char   *front_addr;
  char   *back_addr;
  void   *ptr;
};

struct buffer *buffer_new(size_t item_size,
                          size_t init_capacity,
                          bool fast_prepend) {
  struct buffer *buffer = NULL;

  assert(item_size > 0);
  assert(init_capacity > 0);

  if ((buffer = malloc(sizeof(struct buffer))) == NULL) {
    return NULL;
  }

  if (buffer_init(buffer, item_size, init_capacity, fast_prepend) == NULL) {
    free(buffer);
    return NULL;
  }

  return buffer;
}

struct buffer *buffer_init(struct buffer *buffer,
                           size_t item_size,
                           size_t init_capacity,
                           bool fast_prepend) {
  assert(item_size > 0);
  assert(init_capacity > 0);

  if ((buffer->ptr = malloc(item_size * init_capacity)) == NULL) {
    return NULL;
  }

  buffer->fast_prepend = fast_prepend;
  buffer->item_size = item_size;
  buffer->alloc_len = init_capacity;

  if (fast_prepend) {
    buffer->front_addr = buffer->back_addr =
      (char *) buffer->ptr + (init_capacity / 2) * item_size;
  } else {
    buffer->front_index = buffer->back_index = 0;
  }

  return buffer;
}

void buffer_free(struct buffer *buffer, DtorFunc *destructor) {
  char *ptr = buffer_get(buffer, 0);

  for (size_t i = 0; i < (const size_t) buffer_length(buffer); i++) {
    if (destructor) {
      destructor(ptr);
    }

    ptr += buffer->element_size;
  }

  free(buffer->ptr);
  free(buffer);
}

void *buffer_get(struct buffer *buffer, size_t index) {
  if (index >= buffer_length(buffer)) {
    return NULL;
  } else {
    return (char *) buffer->ptr +
      ((buffer->front_index + index) * buffer->element_size);
  }
}

void buffer_set(struct buffer *buffer, size_t index, void *element) {
  memcpy(buffer_get(buffer, index), element, buffer->element_size);
}

void buffer_insert(struct buffer *buffer, size_t index, void *element);

bool buffer_remove(struct buffer *buffer, size_t index, DtorFunc *destructor) {
  void *addr = NULL, *next_addr = NULL;
  size_t real_index = buffer->front_index + index;

  assert(real_index <= buffer->back_index);

  if ((addr = buffer_get(buffer, index)) == NULL) {
    return false;
  }

  if (destructor) {
    destructor(addr);
  }

  if ((next_addr = buffer_get(buffer, index + 1)) == NULL) {
    return false;
  }

  if (real_index == 0) {
    buffer->front_index++;
  } else if (real_index == buffer->back_index) {
    buffer->back_index--;
  } else {
    memmove(addr, next_addr, (char *) buffer->ptr +
            (buffer->back_index * buffer->element_size) - (char *) next_addr);
    buffer->back_index--;
  }

  return true;
}

size_t buffer_length(struct buffer *buffer) {
  return (buffer->back_index - buffer->front_index) / buffer->element_size;
}

/* struct buffer *buffer_new(int initial_len, size_t element_size) { */
/*   struct buffer *buf = NULL; */
/*  */
/*   assert(initial_len > 0); */
/*   assert(element_size > 0); */
/*  */
/*   if ((buf = malloc(sizeof(struct buffer))) == NULL) { */
/*     return NULL; */
/*   } */
/*  */
/*   if ((buf->ptr = malloc(initial_len * element_size)) == NULL) { */
/*     free(buf); */
/*     return NULL; */
/*   } */
/*  */
/*   buf->allocated_len = initial_len; */
/*   buf->element_size = element_size; */
/*   buf->len = 0; */
/*  */
/*   return buf; */
/* } */
/*  */
/* struct buffer *buffer_append(struct buffer *buf, void *element) { */
/*   void *new_ptr = NULL; */
/*   char *buf_ptr = NULL; */
/*  */
/*   assert(buf != NULL); */
/*   assert(buf->ptr != NULL); */
/*   assert(element != NULL); */
/*  */
/*   if (buf->len == buf->allocated_len) { */
/*     if ((new_ptr = realloc */
/*          (buf->ptr, buf->allocated_len * buf->element_size * 2)) == NULL) { */
/*       return NULL; */
/*     } else { */
/*       buf->ptr = new_ptr; */
/*       buf->allocated_len *= 2; */
/*     } */
/*   } */
/*  */
/*   buf_ptr = buf->ptr; */
/*   memcpy(buf_ptr + (buf->len * buf->element_size / sizeof(char)), */
/*          element, buf->element_size); */
/*   buf->len++; */
/*  */
/*   return buf; */
/* } */
