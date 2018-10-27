#pragma once

#include <stdbool.h>
#include <stdlib.h>

struct char_buffer {
  char   *ptr;
  size_t  len;
  size_t  cap;
  size_t  chunk;
};

void  char_buffer_init(struct char_buffer *const self, const size_t chunk);
void  char_buffer_free(struct char_buffer *const self);
void  char_buffer_revn(struct char_buffer *const self, const size_t n);
bool  char_buffer_extn(struct char_buffer *const self, const size_t n);
char *char_buffer_addn(struct char_buffer *const self, const size_t n)
  __attribute__((malloc, warn_unused_result));
