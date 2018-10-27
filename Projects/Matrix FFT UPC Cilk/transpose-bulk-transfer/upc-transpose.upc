#include <stdio.h>
#include "gettime.h"
#include "utils.h"

#define BUPC_USE_UPC_NAMESPACE

#include <upc.h>
#include <bupc_extensions.h>

#define N  MAT_WIDTH
#define T  MYTHREAD
#define NT THREADS
/* TODO: N/(NT*2) */
#define BS N/NT
/* #define BS 1 */
#define ILS BS
#define JLS BS

#define SOFL sizeof(float)
#define RUNS 1

shared [BS] float m[N][N];
int swaps;

shared void *srclist[JLS];

void check_matrix()
{
#ifdef DO_CHECK
  int i, j;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (m[j][i] != (float)(i * N + j)) {
	printf("Error.\n");
	return;
      }
  printf("Success.\n");
#endif /* DO_CHECK */
}

void init()
{
  if (T == 0) {
    init_matrix(m, N, N);
    print_matrix(m, N, N);
    /* print_affinity(m, N, N); */
  }
}

void fin()
{
  upc_barrier;

  if (T == 0) {
    print_matrix(m, N, N);
    check_matrix();
  }
}

void transpose_remote_out(int i0, int i1, int j0, int j1)
{
  int i, j;
  void *dstlist[JLS];
  float
    tmp,
    *row,
    m2[JLS][ILS];

  /* === READ IN BUFFER === */
  /* m[] are addrs, no remote calls */
  for (j = j0; j < j1; j++) {
    srclist[j - j0] = m[j];
    dstlist[j - j0] = m2[j - j0];
  }

  upc_memget_ilist(j1 - j0, dstlist, SOFL * (i1 - i0),
  		   j1 - j0, srclist, SOFL * (i1 - i0));

  if (T == 1)
    print_matrix(m2, JLS, ILS);

  for (i = i0; i < i1; i++)
    for (j = j0; j < j1; j++)
      if (j > i) {
  	tmp = m[i][j];
  	m[i][j] = m2[j - j0][i - i0];
  	m2[j - j0][i - i0] = tmp;
  	swaps++;
      }

  if (T == 1)
    print_matrix(m2, JLS, ILS);

  /* === WRITE OUT BUFFER === */
  upc_memput_ilist(j1 - j0, srclist, SOFL * (i1 - i0),
  		   j1 - j0, dstlist, SOFL * (i1 - i0));
}

void transpose_in(int i0, int i1, int j0, int j1)
{
  int i, j;
  float tmp;

  for (i = i0; i < i1; i++)
    for (j = j0; j < j1; j++)
      if (j > i) {
  	tmp = m[i][j];
  	m[i][j] = m[j][i];
  	m[j][i] = tmp;
  	swaps++;
      }
}

void transpose(int i0, int i1, int j0, int j1)
{
  int
    di = i1 - i0,
    dj = j1 - j0;

  if (di >= dj && di > ILS) {
    int mi = (i0 + i1) / 2;

    transpose(i0, mi, j0, j1);
    transpose(mi, i1, j0, j1);
  }
  else if (dj > JLS) {
    int mj = (j0 + j1) / 2;

    transpose(i0, i1, j0, mj);
    transpose(i0, i1, mj, j1);
  }
  else if (i0 == j0)
    transpose_in(i0, i1, j0, j1);
  else
    transpose_remote_out(i0, i1, j0, j1);
}

int main (int argc, char **argv)
{
  int t, r, j;
  timespec_t start, end;
  double time_diff;

  init();
  upc_barrier;

  for (r = 0; r < RUNS; r++) {
    swaps = 0;
    get_time_nsec(&start);

    for (j = T * BS; j < N; j += NT * BS)
      transpose(0, j + BS, j, j + BS);

    /* upc_forall (t = 0; t < NT; t++; t) */
    /* transpose(T * BS, (T * BS) + BS, 0, N); */

    get_time_nsec(&end);
    time_diff = time_diff_nsec(&start, &end);

    if (r == RUNS - 1) {
      upc_barrier;
      printf("T %2d: %15.10f s, %d swaps\n", T, time_diff, swaps);
    }

    upc_barrier;
  }

  fin();
  return 0;
}
