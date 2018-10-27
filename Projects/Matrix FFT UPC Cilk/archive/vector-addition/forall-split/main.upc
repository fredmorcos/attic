#include <stdio.h>
#include <upc.h>
#include "gettime.h"

#define N VECLEN
#define T MYTHREAD
/* #define BLOCKSIZE N/THREADS */
#define BLOCKSIZE 1
#define RUNS 2

shared [BLOCKSIZE] float v1[N], v2[N], v3[N];

extern void add_vectors(float *v1, float *v2, float *v3, int i0, int i1);

int main (int argc, char **argv)
{
  int i, i0, i1, r;
  timespec_t start, end;
  double time_diff;
  shared [BLOCKSIZE] float *_v1, *_v2, *_v3;

  if (T == 0) {
    for (i = 0; i < N; i++) {
      v1[i] = (float) i;
      v2[i] = (float) i;
    }
  }

  /* i0 = BLOCKSIZE * T; */
  /* i1 = MIN(BLOCKSIZE * (T+1), N); */

  /* for (i = i0; i < i1; i++) { */
  /*   v1[i] = (float) i; */
  /*   v2[i] = (float) i; */
  /* } */

  upc_barrier;

  for (r = 0; r < RUNS; r++) {
    get_time_nsec(&start);

    upc_forall(i = 0; i < THREADS; i++; i) {
      i0 = BLOCKSIZE * T;
      /* i1 = MIN(BLOCKSIZE * (T+1), N); */
      i1 = MIN((N/THREADS) * (T+1), N);

      _v1 = v1 + i0;
      _v2 = v2 + i0;
      _v3 = v3 + i0;

      add_vectors((float *) _v1, (float *) _v2, (float *) _v3, i0, i1);
    }

    get_time_nsec(&end);
    time_diff = time_diff_nsec(&start, &end);

    if (r == RUNS - 1) {
      printf("T %2d: %.10f s\n", T, time_diff);
      /* printf("%.10f gflops\n", (((double) (i1 - i0)) / time_diff) / 1.e9); */
    }

    upc_barrier;
  }

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
