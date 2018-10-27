#include <stdio.h>

#define BUPC_USE_UPC_NAMESPACE

#include <upc.h>
#include <bupc_extensions.h>
#include "gettime.h"

#define N  MAT_WIDTH
#define T  MYTHREAD
#define NT THREADS
#define BS N/NT

#define RUNS 3

#define FLSIZE sizeof(float)
#define CHSIZE sizeof(char)

shared [BS] float m[N][N];
shared      void  *srclist1[N], *srclist2[N];

#ifdef SHOW_MATRIX
void print_matrix()
{
  int i, j;
  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++)
      printf("%5.1f ", m[i][j]);
    printf("\n");
  }
}
#endif /* SHOW_MATRIX */

void init()
{
  int i, j;
  if (T == 0)
    for (i = 0; i < N; i++)
      for (j = 0; j < N; j++)
	m[i][j] = (float)(i * N + j);
}

int main (int argc, char **argv)
{
  int i, j, i0, i1, j0, j1, r, t;
  timespec_t start, end;
  double time_diff;
  float tmp[N][N];
  upc_handle_t hdla, hdlb;
  void *dstlist1[N], *dstlist2[N];
  float *row;

  init();
  upc_barrier;

  for (r = 0; r < RUNS; r++) {
    get_time_nsec(&start);

    if (T == 0) {
      /* init tmp with zeroes */
      for (i = 0; i < N; i++)
	for (j = 0; j < N; j++)
	  tmp[i][j] = 0.0;

      /* for (i = 0; i < N; i++) */
      /* 	for (j = 0; j < N; j++) */
      /* 	  tmp[i][j] = m[i][j]; */

      /* ====== MEMGET AND MEMPUT TOYS ====== */
      /* copy over the stuff from another thread */
      /* for (i = 0; i < N; i++) { */
      /* 	upc_memget(tmp[i], m[i], */
      /* 		   (FLSIZE / CHSIZE) * (N / 2)); */
      /* 	upc_memget(&tmp[i][N / 2], &m[i][N / 2], */
      /* 		   (FLSIZE / CHSIZE) * (N / 2)); */
      /* } */

      /* collectively copy over the stuff from another thread */
      /* for (i = 0; i < N; i++) { */
      /*   hdla = upc_memget_async(tmp[i], m[i], */
      /* 				(FLSIZE / CHSIZE) * (N / 2)); */
      /*   hdlb = upc_memget_async(&tmp[i][N / 2], &m[i][N / 2], */
      /* 				(FLSIZE / CHSIZE) * (N / 2)); */
      /* } */

      /* upc_waitsync(hdla); */
      /* upc_waitsync(hdlb); */

      /* copy over stuff in a single transfer */
      /* INIT */
      for (i = 0; i < N; i++) {
      	srclist1[i] = m[i];
      	srclist2[i] = &m[i][N / 2];
      	dstlist1[i] = tmp[i];
      	dstlist2[i] = &tmp[i][N / 2];
      }

      /* SYNC */
      /* upc_memget_ilist(N, dstlist1, FLSIZE * (N / 2), */
      /* 		       N, srclist1, FLSIZE * (N / 2)); */
      /* upc_memget_ilist(N, dstlist2, FLSIZE * (N / 2), */
      /* 		       N, srclist2, FLSIZE * (N / 2)); */

      /* ASYNC */
      hdla = upc_memget_ilist_async(N, dstlist1, FLSIZE * (N / 2),
				    N, srclist1, FLSIZE * (N / 2));
      hdlb = upc_memget_ilist_async(N, dstlist2, FLSIZE * (N / 2),
				    N, srclist2, FLSIZE * (N / 2));
      upc_waitsync(hdla);
      upc_waitsync(hdlb);

      /* ====== END MEMGET AND MEMPUT TOYS ====== */
    }

    get_time_nsec(&end);
    time_diff = time_diff_nsec(&start, &end);

    if (r == RUNS - 1) {
      printf("T %2d: %15.10f s\n", T, time_diff);
    }

    upc_barrier;
  }

#ifdef SHOW_MATRIX
  if (T == 0) {
    for (i = 0; i < N; i++) {
      for (j = 0; j < N / 2; j++) {
	row = dstlist1[i];
	printf("%5.1f ", row[j]);
      }
      printf("\n");
    }
    printf("===\n");

    for (i = 0; i < N; i++) {
      for (j = 0; j < N / 2; j++) {
	row = dstlist2[i];
	printf("%5.1f ", row[j]);
      }
      printf("\n");
    }
    printf("===\n");

    print_matrix();

    /* print tmp */
    printf("===\n");
    for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
	printf("%5.1f ", tmp[i][j]);
      }
      printf("\n");
    }
  }
#endif /* SHOW_MATRIX */

  return 0;
}
