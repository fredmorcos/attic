#include <stdio.h>

#define BUPC_USE_UPC_NAMESPACE
#include <upc.h>

#include "utils.h"
#include "common.h"
#include "transpose.h"

shared [BS] double m[N][M];
shared double time_diffs[NT];
shared int swaps[NT];

int main (int argc, char **argv)
{
  init(m, N, M);
  upc_barrier;
  transpose(m, (double *) &(time_diffs[T]), (int *) &(swaps[T]));
  finish(m, N, M);
  return 0;
}
