#pragma once

#include <stdlib.h>

struct string {
  char *ptr;                            /* pointer */
  size_t len;                           /* used length */
  size_t alen;                          /* allocated length */
  size_t clen;                          /* chunk length */
};

typedef struct string String;

#define AutoString __attribute__((cleanup(string_free))) String

void string_init(String *const s, const size_t chunk_len);
void string_free(String *const s);

void  string_revert(String *const s, const size_t n);
char *string_extend(String *const s, const size_t n)
  __attribute_warn_unused_result__ __attribute_malloc__;
char *string_extend_zero(String *const s, const size_t n)
  __attribute_warn_unused_result__ __attribute_malloc__;
