#include "deque.h"
#include "bench.h"
#include <stdio.h>
#include <stdlib.h>

#define SIZE 10000000

#define FORMAT "%90s:%10lfs\n"
#define BENCH(f) do {				\
    starttime();				\
    f;						\
    t = endtime();				\
    printf(FORMAT, #f, t);			\
  } while (0);

void
build_deque(deque_t *deque)
{
  int i, *v1, *v2;

  for (i = 0; i < SIZE; i++)
    {
      v1 = (int *) malloc(sizeof(int));
      v2 = (int *) malloc(sizeof(int));

      *v1 = i;
      *v2 = i;

      deque_push_left(deque, v1);
      deque_push_right(deque, v2);
    }
}

void
pop_deque(deque_t *deque)
{
  int i;

  for (i = 0; i < SIZE / 2; i++)
    {
      free(deque_pop_left(deque));
      free(deque_pop_right(deque));
    }
}

void
rotate_deque(deque_t *deque)
{
  int i;

  for (i = 0; i < SIZE / 2; i++)
    deque_rotate_left(deque);
  
  for (i = 0; i < SIZE / 2; i++)
    deque_rotate_right(deque);
}

int
main (int argc, char **argv)
{
  deque_t *deque = deque_new();
  double t = 0.0;

  BENCH(build_deque(deque));
  BENCH(pop_deque(deque));
  BENCH(rotate_deque(deque));
  BENCH(deque_free(deque));
  return 0;
}
