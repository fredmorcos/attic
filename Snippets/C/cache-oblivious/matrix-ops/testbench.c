#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "matrix.h"
#include "bench.h"

#define INFOFORMAT "%100s: %-50s\n"
#define INFOPRINT(label, value) printf(INFOFORMAT, label, value)

#define BENCHFORMAT "%100s: %-50lf\n"
#define BENCHPRINT(label, time) printf(BENCHFORMAT, label, time)
#define BENCH(f) do {				\
    starttime();				\
    f;						\
    t = endtime();				\
    BENCHPRINT(#f, t);				\
  } while (0);

int
main (int argc, char *argv[])
{
  matrix_t *m1 = NULL, *m2 = NULL, *m3 = NULL;
  double t;
  int
    leaf_size = 16,
    matrix_size = 1000,
    transpose = 1,
    multiply = 0,
    i = 0;
  char
    *leaf_size_str = "16"/* , */
    /* *matrix_size_str = "1000" */;

  for (i = 0; i < argc; i++)
    if (strcmp(argv[i], "--leaf-size") == 0)
      {
	leaf_size = atoi(argv[++i]);
	leaf_size_str = argv[i];
      }
    else if (strcmp(argv[i], "--matrix-size") == 0)
      {
	matrix_size = atoi(argv[++i]);
	/* matrix_size_str = argv[i]; */
      }
    else if (strcmp(argv[i], "--transpose") == 0)
      {
	transpose = 1;
	multiply = 0;
      }
    else if (strcmp(argv[i], "--multiply") == 0)
      {
	multiply = 1;
	transpose = 0;
      }

  /* INFOPRINT("build_flags", __CFLAGS__); */
  INFOPRINT("leaf_size", leaf_size_str);
  /* INFOPRINT("matrix_size", matrix_size_str); */
  /* puts(""); */

  m1 = matrix_new(matrix_size, matrix_size);
  matrix_fill(m1);

  /* MATRIX_PRINT(m1); */

  if (transpose)
    {
      /* BENCH(matrix_transpose(m1)); */
      /* MATRIX_PRINT(m1); */
      /* matrix_stream_to_file(m1, "m1"); */

      /* BENCH(matrix_transpose_recursive(m1, 0, 0, */
      /* 				       m1->height, m1->width, */
      /* 				       leaf_size)); */
      /* MATRIX_PRINT(m1); */
      /* matrix_stream_to_file(m1, "m2"); */

      BENCH(matrix_transpose_recursive_fast(m1, 0, 0,
					    m1->height, m1->width,
					    leaf_size));
      /* MATRIX_PRINT(m1); */
      /* matrix_stream_to_file(m1, "m3"); */

      /* BENCH(matrix_transpose_recursive_simple(m1, 0, 0, */
      /* 					      m1->height, m1->width)); */
      /* MATRIX_PRINT(m1); */

      if (multiply) puts("");
    }

  if (multiply)
    {
      m2 = matrix_new(matrix_size, matrix_size);
      m3 = matrix_new(matrix_size, matrix_size);
      matrix_fill(m2);
      matrix_reset(m3);

      /* MATRIX_PRINT(m1); */
      /* MATRIX_PRINT(m2); */

      BENCH(matrix_multiply(m1, m2, m3));
      /* MATRIX_PRINT(m3); */

      matrix_reset(m3);
      BENCH(matrix_multiply_recursive(m1, m2, m3, 0, 0, 0,
				      m1->height, m2->width, m1->height,
				      leaf_size));
      /* MATRIX_PRINT(m3); */

      matrix_reset(m3);
      BENCH(matrix_multiply_recursive_simple(m1, m2, m3, 0, 0, 0,
					     m1->height, m2->width, m1->height));
      MATRIX_PRINT(m1);
      MATRIX_PRINT(m2);
      MATRIX_PRINT(m3);

      matrix_free(m2);
      matrix_free(m3);
    }

  matrix_free(m1);
  return 0;
}
