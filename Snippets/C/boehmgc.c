/*
 * clang -Wall -Wextra -pedantic -std=c11 -lgc boehmgc.c -o gctest
 */

#include <stdio.h>
#include <stdlib.h>
#include <gc.h>

struct foo {
  int x;
  int y;
  double z;
};

struct foo *extra(void) {
  struct foo *g = GC_malloc(sizeof(struct foo));

  g->x = 0;
  g->y = 1;
  g->z = 2.4;

  return g;
}

void extra2(void) {
  struct foo *g = GC_malloc(sizeof(struct foo));

  g->x = 0;
  g->y = 1;
  g->z = 2.4;
}

int main(void) {
  GC_init();

  struct foo *f = GC_malloc(sizeof(struct foo));

  f->x = 0;
  f->y = 1;
  f->z = 1.5;

  struct foo *g = extra();
  extra2();

  printf("%d %d %f\n", f->x, f->y, f->z);
  printf("%d %d %f\n", g->x, g->y, g->z);

  return 0;
}
