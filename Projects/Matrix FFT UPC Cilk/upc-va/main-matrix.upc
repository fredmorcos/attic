#include <stdio.h>
#include <math.h>
#include "gettime.h"

#define BUPC_USE_UPC_NAMESPACE
#include <upc.h>
#include <bupc_extensions.h>

#define N  VEC_LEN
#define T  MYTHREAD
#define NT THREADS

#define MIN(a,b) (a < b ? a : b)

/* note: (index / BS) % NT = MYTHREAD */

/* source and destination vectors */
shared [BS] double mata[N][N];
shared [BS] double matb[N][N];
shared [BS] double matc[N][N];

#if defined (BULK_COPY) || defined (BULK_COPY_ASYNC)
shared void *matblock_a_srclist[N];
shared void *matblock_b_srclist[N];
shared void *matblock_c_dstlist[N];
#endif /* BULK_COPY || BULK_COPY_ASYNC */

/* per thread-specific stuff */
shared double time_diffs[NT];

void print_mat (shared [BS] double mat[][N])
{
#ifdef SHOW_VEC
  int i, j;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++)
      printf("%5.1f ", mat[i][j]);
    printf("\n");
  }
  printf("\n");

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++)
      printf("%5d ", (int) upc_threadof(&mat[i][j]));
    printf("\n");
  }
  printf("\n");
#endif /* SHOW_VEC */
}

int main (int argc, char **argv)
{
  timespec_t start, end;
  int
    i,      /* vector elements or "partition" iterator */
    j,      /* vector "partition elements" iterator */
    thread, /* threads iterator */
    run;    /* number of runs iterator */

  /* fill and print the source matrixes */
  if (T == 0) {
    for (i = 0; i < N; i++)
      for (j = 0; j < N; j++)
	mata[i][j] = matb[i][j] = (double) (i * N + j);

    print_mat(mata);
    print_mat(matb);
  }

  upc_barrier;

  for (run = 0; run < RUNS; run++) {
    get_time_nsec (&start);

#if defined (TEST)
    if (T == 0)
      for (i = 0; i < N; i++)
	for (j = 0; j < N; j++)
	  matc[i][j] = mata[i][j] + matb[i][j];
#elif defined (BULK_COPY)
#define LBS (sizeof (double) * BS)
    if (T == 0) {
      int si, sj;
      double matblock_a[N][BS], matblock_b[N][BS], matblock_c[N][BS];
      void *matblock_a_dstlist[N];
      void *matblock_b_dstlist[N];
      void *matblock_c_srclist[N];

      for (j = 0; j < N; j += BS) {
	for (i = 0; i < N; i++) {
	  matblock_a_srclist[i] = &mata[i][j];
	  matblock_a_dstlist[i] = &matblock_a[i][0];
	  /* matblock_b_srclist[i] = &matb[i][j]; */
	  /* matblock_b_dstlist[i] = matblock_b[i]; */

	  /* matblock_c_srclist[i] = matblock_c[i]; */
	  /* matblock_c_dstlist[i] = &matc[i][j]; */
	}

	upc_memget_ilist(N, matblock_a_dstlist, sizeof(double) / sizeof(char),
			 N, matblock_a_srclist, sizeof(double) / sizeof(char));
	/* upc_memget_ilist(N, matblock_b_dstlist, LBS, */
	/* 		 N, matblock_b_srclist, LBS); */

	for (si = 0; si < N; si++) {
	  for (sj = 0; sj < BS; sj++)
	    printf("%5.1f ", matblock_a[i][j]);
	  printf("\n");
	}

	exit(0);

	/* for (si = 0; si < N; si++) */
	/*   for (sj = 0; sj < BS; sj++) */
	/*     matblock_c[i][j] = matblock_a[i][j] + matblock_b[i][j]; */

	/* upc_memput_ilist(N, matblock_c_dstlist, LBS, */
	/* 		 N, matblock_c_srclist, LBS); */
      }
    }
#else /* METHOD */
    printf("No method specified.\n");
#endif /* METHOD */

    get_time_nsec (&end);
    time_diffs[T] = time_diff_nsec (&start, &end);
    upc_barrier;
  }

  upc_barrier;

  if (T == 0) {
    print_mat (matc);

#ifdef DO_CHECK
    for (i = 0; i < N; i++)
      for (j = 0; j < N; j++)
	if (matc[i][j] != (2.0 * (i * N + j))) {
	  printf("error\n");
	  goto leave;
	}
    printf("success\n");
  leave:
#endif /* DO_CHECK */
  }

  if (T == 0)
    for (thread = 0; thread < NT; thread++)
      printf ("T %2d: %.15f s\n", thread, time_diffs[thread]);

  return 0;
}
