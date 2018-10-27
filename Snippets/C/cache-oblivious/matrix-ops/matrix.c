#include "matrix.h"

#include <stdlib.h>

#define MATRIX_INDEX(matrix, i, j) (i * matrix->width + j)
#define MATRIX_DATA(matrix, i, j) (matrix->data[MATRIX_INDEX(matrix, i, j)])

matrix_t *
matrix_new(int width, int height)
{
  matrix_t *tmp = (matrix_t *) malloc(sizeof(matrix_t));
  tmp->width = width;
  tmp->height = height;
  tmp->data = (double *) malloc(sizeof(double) * width * height);
  return tmp;
}

matrix_t *
matrix_new_from_stream(FILE *stream)
{
  matrix_t *tmp;
  int width, height, i, j;

  fscanf(stream, "%d %d", &width, &height);
  tmp = matrix_new(width, height);

  for (i = 0; i < height; i++)
    for (j = 0; j < width; j++)
      fscanf(stream, "%lf", &MATRIX_DATA(tmp, i, j));

  return tmp;
}

matrix_t *
matrix_new_from_file(const char *filename)
{
  FILE *input = fopen(filename, "r");
  matrix_t *tmp = matrix_new_from_stream(input);
  fclose(input);
  return tmp;
}

void
matrix_free(matrix_t *matrix)
{
  free(matrix->data);
  free(matrix);
}

double
matrix_get(matrix_t *matrix, int i, int j)
{
  return MATRIX_DATA(matrix, i, j);
}

void
matrix_set(matrix_t *matrix, int i, int j, double value)
{
  MATRIX_DATA(matrix, i, j) = value;
}

void
matrix_reset(matrix_t *matrix)
{
  int i, j;

  for (i = 0; i < matrix->width; i++)
    for (j = 0; j < matrix->height; j++)
      MATRIX_DATA(matrix, i, j) = 0.0;
}

void
matrix_fill(matrix_t *matrix)
{
  int i, j;
  double k = 0.0;

  for (i = 0; i < matrix->width; i++)
    for (j = 0; j < matrix->height; j++)
      MATRIX_DATA(matrix, i, j) = k++;
}

void
matrix_stream(matrix_t *matrix, FILE *stream, int info)
{
  int i, j;

  if (info)
    fprintf(stream, "%d %d\n", matrix->width, matrix->height);

  for (i = 0; i < matrix->height; i++)
    {
      for (j = 0; j < matrix->width; j++)
	fprintf(stream, "%12lf ", MATRIX_DATA(matrix, i, j));
      fprintf(stream, "\n");
    }
}

void
matrix_stream_to_file(matrix_t *matrix, const char *filename)
{
  FILE *output = fopen(filename, "w+");
  matrix_stream(matrix, output, 1);
  fclose(output);
}

void
matrix_transpose(matrix_t *matrix)
{
  int i, j;
  double s;

  for (i = 0; i < matrix->height - 1; i++)
    {
      for (j = i + 1; j < matrix->width; j++)
	{
	  s = MATRIX_DATA(matrix, i, j);
	  MATRIX_DATA(matrix, i, j) = MATRIX_DATA(matrix, j, i);
	  MATRIX_DATA(matrix, j, i) = s;
	}
    }
}

void
matrix_transpose_recursive(matrix_t *matrix, int i0, int j0, int i1, int j1, int ls)
{
  int di = i1 - i0,
      dj = j1 - j0;
  double s;

  if (di >= dj && di > ls)
    {
      matrix_transpose_recursive(matrix, i0, j0, (i0 + i1) / 2, j1, ls);
      matrix_transpose_recursive(matrix, (i0 + i1) / 2, j0, i1, j1, ls);
    }
  else if (dj > ls)
    {
      matrix_transpose_recursive(matrix, i0, j0, i1, (j0 + j1) / 2, ls);
      matrix_transpose_recursive(matrix, i0, (j0 + j1) / 2, i1, j1, ls);
    }
  else
    {
      int i, j;

      for (i = i0; i < i1; i++)
	{
	  for (j = j0; j < j1; j++)
	    {
	      if (i >= j)
		continue;

	      s = MATRIX_DATA(matrix, i, j);
	      MATRIX_DATA(matrix, i, j) = MATRIX_DATA(matrix, j, i);
	      MATRIX_DATA(matrix, j, i) = s;
	    }
	}
    }
}

void
matrix_transpose_recursive_fast(matrix_t *matrix, int i0, int j0, int i1, int j1, int ls)
{
  int dj = j1 - j0, di = i1 - i0;

  if (di >= dj && di > ls) {
      int i = (i0 + i1) / 2;

      if (j0 >= i0)
	matrix_transpose_recursive_fast(matrix, i0, j0, i, j1, ls);

      if (j1 >= i)
	matrix_transpose_recursive_fast(matrix, i, j0, i1, j1, ls);
    }
  else if (dj > ls) {
      int j = (j0 + j1) / 2;

      if (i0 <= j0)
	matrix_transpose_recursive_fast(matrix, i0, j0, i1, j, ls);

      if (i0 <= j)
	matrix_transpose_recursive_fast(matrix, i0, j, i1, j1, ls);
    }
  else {
      /* if (i0 > j0) return; */

      int i, j;
      double s;

      for (i = i0; i < i1; i++) {
	  for (j = (i0 == j0 ? i + 1 : j0); j < j1; j++) {
	      s = MATRIX_DATA(matrix, i, j);
	      MATRIX_DATA(matrix, i, j) = MATRIX_DATA(matrix, j, i);
	      MATRIX_DATA(matrix, j, i) = s;
	    }
	}
    }
}

void
matrix_transpose_recursive_simple(matrix_t *matrix, int i0, int j0, int i1, int j1)
{
  int di = i1 - i0,
      dj = j1 - j0;

  if (di == 1 && dj == 1)
    {
      if (i0 >= j0) return;

      double s = MATRIX_DATA(matrix, i0, j0);
      MATRIX_DATA(matrix, i0, j0) = MATRIX_DATA(matrix, j0, i0);
      MATRIX_DATA(matrix, j0, i0) = s;
    }
  else
    {
      if (di >= dj)
	{
	  int im = (i0 + i1) / 2;
	  matrix_transpose_recursive_simple(matrix, i0, j0, im, j1);
	  matrix_transpose_recursive_simple(matrix, im, j0, i1, j1);
	}
      else
	{
	  int jm = (j0 + j1) / 2;
	  matrix_transpose_recursive_simple(matrix, i0, j0, i1, jm);
	  matrix_transpose_recursive_simple(matrix, i0, jm, i1, j1);
	}
    }
}

void
matrix_multiply(matrix_t *matrix1, matrix_t *matrix2, matrix_t *matrix3)
{
  int i, j, k;

  for (i = 0; i < matrix1->width; i++)
    for (j = 0; j < matrix2->height; j++)
      for (k = 0; k < matrix1->height; k++)
	MATRIX_DATA(matrix3, i, j) += (MATRIX_DATA(matrix1, i, k) * 
				       MATRIX_DATA(matrix2, k, j));
}

void
matrix_multiply_recursive(matrix_t *matrix1, matrix_t *matrix2, matrix_t *matrix3,
			  int i0, int j0, int k0, int i1, int j1, int k1, int ls)
{
  int di = i1 - i0,
      dj = j1 - j0,
      dk = k1 - k0;

  if (di >= dj && di >= dk && di > ls)
    {
      int i = (i0 + i1) / 2;

      matrix_multiply_recursive(matrix1, matrix2, matrix3, i0, j0, k0, i, j1, k1, ls);
      matrix_multiply_recursive(matrix1, matrix2, matrix3, i, j0, k0, i1, j1, k1, ls);
    }
  else if (dj >= di && dj >= dk && dj > ls)
    {
      int j = (j0 + j1) / 2;

      matrix_multiply_recursive(matrix1, matrix2, matrix3, i0, j, k0, i1, j1, k1, ls);
      matrix_multiply_recursive(matrix1, matrix2, matrix3, i0, j0, k0, i1, j, k1, ls);
    }
  else if (dk >= di && dk >= dj && dk > ls)
    {
      int k = (k0 + k1) / 2;

      matrix_multiply_recursive(matrix1, matrix2, matrix3, i0, j0, k, i1, j1, k1, ls);
      matrix_multiply_recursive(matrix1, matrix2, matrix3, i0, j0, k0, i1, j1, k, ls);
    }
  else
    {
      int i, j, k;

      for (i = i0; i < i1; i++)
	for (j = j0; j < j1; j++)
	  for (k = k0; k < k1; k++)
	    MATRIX_DATA(matrix3, i, j) += (MATRIX_DATA(matrix1, i, k) *
					   MATRIX_DATA(matrix2, k, j));
    }
}

void
matrix_multiply_recursive_simple(matrix_t *matrix1, matrix_t *matrix2, matrix_t *matrix3,
				 int i0, int j0, int k0, int i1, int j1, int k1)
{
  int di = i1 - i0,
      dj = j1 - j0,
      dk = k1 - k0;

  if (di == 1 && dj == 1 && dk == 1)
    MATRIX_DATA(matrix3, i0, j0) += (MATRIX_DATA(matrix1, i0, k0) *
				     MATRIX_DATA(matrix2, k0, j0));
  else
    {
      if (di >= dj && di >= dk)
	{
	  int im = (i0 + i1) / 2;
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   i0, j0, k0, im, j1, k1);
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   im, j0, k0, i1, j1, k1);
	}
      else if (dj >= dk)
	{
	  int jm = (j0 + j1) / 2;
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   i0, j0, k0, i1, jm, k1);
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   i0, jm, k0, i1, j1, k1);
	}
      else
	{
	  int km = (k0 + k1) / 2;
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   i0, j0, k0, i1, j1, km);
	  matrix_multiply_recursive_simple(matrix1, matrix2, matrix3,
					   i0, j0, km, i1, j1, k1);
	}
    }
}
