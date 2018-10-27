#include <stdio.h>
#include <cilk/cilk.h>
#include "bench.h"

int do_something()
{
  return 1;
}

int main (int argc, char **argv)
{
  double t = 0;
  int y = 0;

  printf("nworkers = %d\n", __cilkrts_get_nworkers());
  printf("total workers = %d\n", __cilkrts_get_total_workers());

  starttime();
  /* y = cilk_spawn do_something(); */
  /* cilk_sync; */
  t = endtime();

  printf("time = %fs\n", t);
  return 0;
}
