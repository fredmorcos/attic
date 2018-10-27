#include "transpose.h"
#include "common.h"
#include "gettime.h"

#define BUPC_USE_UPC_NAMESPACE
#include <upc.h>
#include <bupc_extensions.h>

void transpose (shared [BS] double m[N][M], double *time_diff, int *swaps)
{
  int i, j, r, sw = *swaps;
  double tmp, dt = *time_diff;
  timespec_t start, end;

  for (r = 0; r < RUNS; r++) {
    sw = 0;
    get_time_nsec(&start);

    /* for (j = T * BS; j < N; j += NT * BS) */
    /*   transpose(0, j + BS, j, j + BS); */

    /* upc_forall (t = 0; t < NT; t++; t) */
    /* transpose(T * BS, (T * BS) + BS, 0, N); */

    for (j = T * BS; j < (T * BS) + BS; j++) {
      for (i = 0; i < j; i++) {
	tmp     = m[i][j];
	m[i][j] = m[j][i];
	m[j][i] = tmp;
	sw++;
      }
    }

    get_time_nsec(&end);
    dt = time_diff_nsec(&start, &end);
  }
}
