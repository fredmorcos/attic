#include "ctx_mm.h"

#define SIZE 2000

void test1 (void) {
  struct ctx_mm_t ctx;
  int i = 0;
  char * msg = NULL;

  ctx_mm_init(&ctx);

  for (i = 0; i < SIZE; i++) {
    msg = ctx_malloc(sizeof(char) * SIZE, &ctx);

    if (msg == NULL) {
      break;
    }
  }

  ctx_free(&ctx);
}

int main (void) {
  struct ctx_mm_t ctx;
  int i = 0;
  char * msg = NULL;

  ctx_mm_init(&ctx);

  for (i = 0; i < SIZE; i++) {
    msg = ctx_malloc(sizeof(char) * SIZE, &ctx);

    if (msg == NULL) {
      break;
    }
  }

  for (i = 0; i < SIZE; i++)
    test1();

  ctx_free(&ctx);
  return 0;
}
