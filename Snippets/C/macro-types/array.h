#include <stdio.h>
#include <stdlib.h>

#define Array(TYPE)                                                     \
  struct TYPE##_array {                                                 \
    TYPE *ptr;                                                          \
    size_t len;                                                         \
    size_t alen;                                                        \
  };                                                                    \
                                                                        \
  void TYPE##_array_init(struct TYPE##_array *const a,                  \
                         const size_t len,                              \
                         const size_t ilen);                            \
  void TYPE##_array_free(struct TYPE##_array *const a);                 \
                                                                        \
  void TYPE##_array_init(struct TYPE##_array *const a,                  \
                         const size_t len,                              \
                         const size_t ilen) {                           \
    printf("Init\n");                                                   \
  }                                                                     \
                                                                        \
  void TYPE##_array_free(struct TYPE##_array *const a) {                \
    printf("Free\n");                                                   \
  }
