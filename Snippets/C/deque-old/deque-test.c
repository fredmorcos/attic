#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "deque.h"
#include "bench.h"

#define SIZE 20

#define FORMAT "%90s:%10lfs\n"
#define BENCH(f) do {				\
    starttime();				\
    f;						\
    t = endtime();				\
    printf(FORMAT, #f, t);			\
  } while (0);

int main (int argc, char **argv)
{
  deque_t *d;
  int i, *v1, *v2;
  double t, total = 0.0;

  d = deque_new();
  assert(d->_rsize == CHUNK);
  assert(deque_size(d) == 0);
  assert(deque_is_empty(d) == 1);

  deque_print(d);

  starttime();
  for (i = 0; i < SIZE; i++)
    {
      v1 = (int *) malloc(sizeof(int));
      v2 = (int *) malloc(sizeof(int));
      *v1 = i;
      *v2 = i;
      deque_push_left(d, v1);
      deque_push_right(d, v2);
      deque_print(d);
    }
  t = endtime();
  printf(FORMAT, "allocations", t);

  total += t;

  deque_print(d);

  assert(d->_left < d->_right);
  assert(deque_size(d) == (SIZE * 2));
  assert(deque_is_empty(d) == 0);
  assert(d->_rsize >= deque_size(d));

  BENCH(deque_free(d, free));

  total += t;

  printf(FORMAT, "total", total);

  return 0;
}
