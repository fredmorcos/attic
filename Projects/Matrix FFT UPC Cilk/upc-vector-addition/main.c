#include <stdio.h>
#include "gettime.h"

#define N 200000000

float v1[N], v2[N], v3[N];

int main (int argc, char **argv)
{
  int i;
  timespec_t start, end;
  double time_diff;

  for (i = 0; i < N; i++) {
    v1[i] = (float) i;
    v2[i] = (float) i;
  }

  get_time_nsec(&start);

  for (i = 0; i < N; i++) {
    v3[i] = v1[i] + v2[i];
  }

  get_time_nsec(&end);
  time_diff = time_diff_nsec(&start, &end);

  printf("T %2d: %.10f s, ", 0, time_diff);
  printf("%.10f gflops\n", ((double) N / time_diff) * 1.e-9);

  for (i = 0; i < N; i++)
    if (v3[i] != (float) (i + i)) {
      printf("Error\n");
      return 1;
    }
  printf("Success\n");

  return 0;
}
