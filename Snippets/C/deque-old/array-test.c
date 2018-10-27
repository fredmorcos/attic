#include <stdio.h>
#include <stdlib.h>
#include "array.h"
#include "bench.h"

#define SIZE 20000

#define FORMAT "%90s:%10lfs\n"
#define BENCH(f) do {				\
    starttime();				\
    f;						\
    t = endtime();				\
    printf(FORMAT, #f, t);			\
  } while (0);

int main (int argc, char **argv)
{
  array_t *l = array_new();
  int i, *v;
  double t, total = 0.0;

  starttime();
  for (i = 0; i < SIZE; i++)
    {
      v = (int *) malloc(sizeof(int));
      *v = i;
      array_append(l, v);
    }
  t = endtime();
  printf(FORMAT, "allocations", t);

  total += t;

  starttime();
  for (i = 0; i < SIZE; i++)
    {
      v = (int *) malloc(sizeof(int));
      *v = i;
      array_insert(l, v, SIZE / 2);
    }
  t = endtime();
  printf(FORMAT, "insertions", t);

  total += t;

  BENCH(array_free(l, free));

  total += t;

  printf(FORMAT, "total", total);

  return 0;
}
