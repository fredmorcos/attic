#pragma once
#include <stdlib.h>
#include <stdbool.h>

struct buffer {
  void *ptr;            /* pointer to the start of allocated buffer */
  size_t element_size;  /* size of a single element */
  size_t allocated_len; /* the currently allocated length */
  size_t len;           /* the actual length of data in the buffer */
};

struct buffer *
buffer_init(void *ctx, size_t initial_len, size_t element_size);

size_t
buffer_append(struct buffer *buffer, void *element);

struct buffer *
buffer_reduce(struct buffer *buffer);

__attribute__((always_inline)) inline void *
buffer_index(struct buffer *buffer, size_t index);
