#ifndef __MATRIX_H__
#define __MATRIX_H__

#include <stdio.h>

#define MATRIX_PRINT(matrix) do {		\
    matrix_stream(matrix, stdout, 0);		\
    puts("");					\
  } while (0);

typedef struct _matrix_t
{
  double *data;
  int width, height;
} matrix_t;

matrix_t *matrix_new(int, int);
matrix_t *matrix_new_from_stream(FILE *);
matrix_t *matrix_new_from_file(const char *);

void matrix_free(matrix_t *);

double matrix_get(matrix_t *, int, int);
void matrix_set(matrix_t *, int, int, double);

void matrix_reset(matrix_t *);
void matrix_fill(matrix_t *);
void matrix_stream(matrix_t *, FILE *, int);
void matrix_stream_to_file(matrix_t *, const char *);

void matrix_transpose(matrix_t *);
void matrix_transpose_recursive(matrix_t *, int, int, int, int, int);
void matrix_transpose_recursive_fast(matrix_t *, int, int, int, int, int);
void matrix_transpose_recursive_simple(matrix_t *, int, int, int, int);
void matrix_transpose_recursive_simple_square(matrix_t *, int, int);

void matrix_multiply(matrix_t *, matrix_t *, matrix_t *);
void matrix_multiply_recursive(matrix_t *, matrix_t *, matrix_t *,
			       int, int, int, int, int, int, int);
void matrix_multiply_recursive_simple(matrix_t *, matrix_t *, matrix_t *,
				      int, int, int, int, int, int);

#endif /* __MATRIX_H__ */
