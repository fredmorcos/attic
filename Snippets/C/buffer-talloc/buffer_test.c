#include <talloc.h>
#include "buffer.h"

int main(void) {
  struct foo {
    double x;
    int y;
    float z;
  };

  void *ctx;
  struct buffer *buffer;
  struct foo foo;

  ctx = talloc_autofree_context();
  buffer = buffer_init(ctx, 10, sizeof(struct foo));

  foo = (struct foo) {
    .x = 5.5,
    .y = 5,
    .z = 4.5
  };

  buffer_append(buffer, &foo);

  return 0;
}
