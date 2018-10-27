#include <stdio.h>
#include <upc.h>
#include "gettime.h"

#define N  VECLEN
#define T  MYTHREAD
#define NT THREADS
#define BLOCKSIZE N/NT

shared [BLOCKSIZE] float v1[N], v2[N], v3[N];

int main (int argc, char **argv)
{
  int i, i0, i1;
  timespec_t start, end;
  double time_diff;

  if (T == 0) {
    for (i = 0; i < N; i++) {
      v1[i] = (float) i;
      v2[i] = (float) i;
    }
  }

  upc_barrier;

  get_time_nsec(&start);

  i0 = BLOCKSIZE * T;
  i1 = MIN(BLOCKSIZE * (T+1), N);

  for (i = i0; i < i1; i++)
    v3[i] = v1[i] + v2[i];

  get_time_nsec(&end);
  time_diff = time_diff_nsec(&start, &end);

  printf("T %2d: %.10f s\n", T, time_diff);
  /* printf("%.10f gflops\n", (((double) (i1 - i0)) / time_diff) / 1.e9); */

  upc_barrier;

  if (T == 0) {
    for (i = 0; i < N; i++)
      if (v3[i] != (float) (i + i)) {
	printf("Error\n");
	return 1;
      }
    printf("Success\n");
  }

  return 0;
}
