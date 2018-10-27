#ifndef PET_BUFFER_H
#define PET_BUFFER_H

#include <stdlib.h>

typedef struct {
  void *ptr;            /* pointer to the start of allocated buffer */
  void *ctx;            /* talloc parent context */
  size_t esize;         /* sizeof() a single element */
  int alen;             /* the currently allocated length */
  int len;              /* the actual length of data in the buffer */
} buf_t;

buf_t *buf_init (void *ctx, int ilen, size_t esize);
buf_t *buf_append (buf_t *buf, void *elem);

#endif
