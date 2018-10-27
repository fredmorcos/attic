#include <stdio.h>
#include <upc.h>
#include "gettime.h"

#define N  MAT_WIDTH
#define T  MYTHREAD
#define NT THREADS
#define BS N/NT /* 1, N/NT, (N/NT)+1 */

#define RUNS 3

shared [BS] float m[N][N];

/* extern void add_vectors(float *v1, float *v2, float *v3, int i0, int i1); */
/* extern int transpose(float **m, int width); */

#ifdef DO_CHECK
void check_matrix()
{
  int i, j;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      if (m[j][i] != (float)(i * N + j)) {
	printf("Error.\n");
	return;
      }
    }
  }
  printf("Success.\n");
}
#endif /* DO_CHECK */

#ifdef SHOW_MATRIX
void print_matrix()
{
  int i, j;

  for (i = 0; i < N; i++) {
    for (j = 0; j < N; j++) {
      printf("%5.1f ", m[i][j]);
    }
    printf("\n");
  }
}
#endif /* SHOW_MATRIX */

int main (int argc, char **argv)
{
  int i, j, i0, i1, j0, j1, r, t, it;
  timespec_t start, end;
  double time_diff;
  float tmp;
  /* shared [BS] float *_m; */

  if (T == 0) {
    for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
	m[i][j] = (float)(i * N + j);
      }
    }

#ifdef SHOW_MATRIX
    print_matrix();
#endif /* SHOW_MATRIX */
  }

  upc_barrier;

  for (r = 0; r < RUNS; r++) {
    it = 0;
    get_time_nsec(&start);

    /* /\* upc_forall (j = 1; j < N; j++; j) { *\/ */
    /* upc_forall (j = 1; j < N; j++; ((int)(((double)j)/((double)BS))) % N) { */
    /*   /\* printf("j = %d, BS = %d, j/BS = %d, j/BS = %g, (j/BS)//NT = %d\n", *\/ */
    /*   /\* 	     j, BS, j/BS, ((double)j)/((double)BS), ((int)(((double)j)/((double)BS))) % NT); *\/ */
    /*   for (i = 0; i < j; i++) { */
    /* 	it++; */
    /* 	tmp     = m[i][j]; */
    /* 	m[i][j] = m[j][i]; */
    /* 	m[j][i] = tmp; */
    /*   } */
    /* } */

    /* upc_forall (t = 0; t < NT; t++; t) { */
    /*   for (i = 0; i < N; i++) { */
    /* 	for (j = T*BS; j < (T*BS)+BS; j++) { */
    /* 	  if (i < j) { */
    /* 	    it++; */
    /* 	    tmp     = m[i][j]; */
    /* 	    m[i][j] = m[j][i]; */
    /* 	    m[j][i] = tmp; */
    /* 	  } */
    /* 	} */
    /*   } */
    /* } */

    for (j = T*BS; j < (T*BS)+BS; j++) {
      for (i = 0; i < j; i++) {
    	it++;
    	tmp     = m[i][j];
    	m[i][j] = m[j][i];
    	m[j][i] = tmp;
      }
    }

    get_time_nsec(&end);
    time_diff = time_diff_nsec(&start, &end);

    if (r == RUNS - 1) {
      printf("T %2d: %15.10f s, %d swaps\n", T, time_diff, it);
    }

    upc_barrier;
  }

  if (T == 0) {
#ifdef DO_CHECK
    check_matrix();
#endif /* DO_CHECK */

#ifdef SHOW_MATRIX
    print_matrix();
#endif /* SHOW_MATRIX */
  }

  return 0;
}
