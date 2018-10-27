#include <stdio.h>
#include <cilk/cilk.h>
#include "gettime.h"
#include "defines.h"

void transpose(float *matrix, int i0, int i1, int j0, int j1)
{
  int di = i1 - i0, dj = j1 - j0;

  if (di > LEAF_SIZE_I && di >= dj) {
    int mi = (i0 + i1) / 2;

    cilk_spawn transpose(matrix, i0, mi, j0, j1);
    cilk_spawn transpose(matrix, mi, i1, j0, j1);
  } else if (dj > LEAF_SIZE_J && dj >= di) {
    int mj = (j0 + j1) / 2;

    cilk_spawn transpose(matrix, i0, i1, j0, mj);
    cilk_spawn transpose(matrix, i0, i1, mj, j1);
  } else {
    int i, j;
    float tmp;

    for (i = i0; i < i1; i++) {
      for (j = j0; j < i; j++) {
	/* if (j >= i) */
	/*   continue; */
	tmp = MAT_CELL_1D(matrix, i, j);
	MAT_CELL_1D(matrix, i, j) = MAT_CELL_1D(matrix, j, i);
	MAT_CELL_1D(matrix, j, i) = tmp;
      }
    }
  }
}

#ifdef DO_CHECK
void transpose_simple(float *matrix)
{
  int i, j;
  float tmp;

  for (i = 1; i < MAT_WIDTH; i++) {
    for (j = 0; j < i; j++) {
      tmp = MAT_CELL_1D(matrix, i, j);
      MAT_CELL_1D(matrix, i, j) = MAT_CELL_1D(matrix, j, i);
      MAT_CELL_1D(matrix, j, i) = tmp;
    }
  }
}

int check(float *m1, float *m2)
{
  int i;

  for (i = 0; i < MAT_WIDTH * MAT_HEIGHT; i++) {
    if (m1[i] != m2[i])
      return 1;
  }

  return 0;
}

#ifdef SHOW_MATRIX
void print(float *m)
{
  int i;

  for (i = 0; i < MAT_WIDTH * MAT_HEIGHT; i++)
    printf("%g ", m[i]);
  printf("\n");
}
#endif /* SHOW_MATRIX */
#endif /* DO_CHECK */

void fill(float *matrix)
{
  int i;

  for (i = 0; i < MAT_WIDTH * MAT_HEIGHT; i++)
    matrix[i] = (float) i;
}

#ifndef __UPC
int main (int argc, char **argv)
{
  float matrix[MAT_WIDTH * MAT_HEIGHT];
  timespec_t begin, end;
  double diff;

#ifdef DO_CHECK
  float matrix2[MAT_WIDTH * MAT_HEIGHT];
#endif /* DO_CHECK */

  fill(matrix);

#ifdef DO_CHECK
#ifdef SHOW_MATRIX
  printf("original before:  ");
  print(matrix);
#endif /* SHOW_MATRIX */
#endif /* DO_CHECK */

  get_time_nsec(&begin);

  cilk_spawn transpose(matrix, 0, MAT_WIDTH, 0, MAT_HEIGHT);
  cilk_sync;

  get_time_nsec(&end);
  diff = time_diff_nsec(&begin, &end);

  printf("time = %.10f\n", diff);
  printf("gswaps = %.10f\n",
	 (((double) (MAT_WIDTH * MAT_HEIGHT) / 2.0) / diff) * 1.e-9);

#ifdef DO_CHECK
  fill(matrix2);

#ifdef SHOW_MATRIX
  printf("reference before: ");
  print(matrix2);
#endif /* SHOW_MATRIX */

  transpose_simple(matrix2);

#ifdef SHOW_MATRIX
  printf("original after:   ");
  print(matrix);
  printf("reference after:  ");
  print(matrix2);
#endif /* SHOW_MATRIX */

  if (check(matrix, matrix2)) {
    printf("Error\n");
    return 1;
  }

  printf("Success\n");
#endif /* DO_CHECK */

  return 0;
}
#endif /* ndef __UPC */
