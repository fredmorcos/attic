#include "ctx_mm.h"

#include <assert.h>
#include <stdlib.h>

#define max(a, b) (a >= b ? a : b)

void ctx_mm_init (struct ctx_mm_t * const ctx) {
  assert(ctx != NULL);

  ctx->free_l    = NULL;
  ctx->len       = 0;
  ctx->real_len_ = 0;
}

void * ctx_malloc (const size_t size, struct ctx_mm_t * const ctx) {
  /* TODO
   *
   * Implement a more efficient reallocation scheme for free_l when an
   * efficient list implementation is available.
   */

  assert(ctx != NULL);
  assert(ctx->real_len_ >= ctx->len);
  assert(size > 0);

  void * new_alloc  = NULL;

  if (ctx->len == ctx->real_len_) {
    void   * new_free_l = NULL;
    size_t   alloc_size = max(ctx->real_len_ * 2, 2);

    new_free_l = (void **) realloc(ctx->free_l, sizeof(void **) * alloc_size);

    if (new_free_l == NULL)
      return NULL;

    ctx->real_len_ = alloc_size;
    ctx->free_l = new_free_l;
  }

  new_alloc = malloc(size);

  if (new_alloc != NULL) {
    ctx->free_l[ctx->len] = new_alloc;
    ctx->len += 1;
  }

  return new_alloc;
}

void ctx_free (void * const ptr, struct ctx_mm_t * const ctx) {
  assert(ctx != NULL);
  assert(ctx->free_l != NULL);
  assert(ptr != NULL);
  assert(ctx->len > 0);
  assert(ctx->real_len_ >= ctx->len);

  size_t i = 0;

  for (i = 0; i < ctx->len; i++)
    if (ctx->free_l[i] == ptr) {
      free(ptr);
      ctx->free_l[i] = NULL;
    }
}

void ctx_free_all (struct ctx_mm_t * const ctx) {
  assert(ctx != NULL);
  assert(ctx->free_l != NULL);

  size_t i = 0;

  for (i = 0; i < ctx->len; i++)
    if (ctx->free_l[i] != NULL)
      free(ctx->free_l[i]);

  free(ctx->free_l);
}
