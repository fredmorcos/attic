#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include "dft.h"
#include "gettime.h"

#define LS 7

void populate (fftw_complex *m, int size)
{
  int i;

  for (i = 0; i < size; i++)
    m[i] = ((float) i) + 0.0 * I;
}

void print (fftw_complex *m, int w, int h)
{
  int i, j;

  for (j = 0; j < h; j++)
    {
      for (i = 0; i < w; i++)
	{
	  printf("(%f + i%f) ", crealf(m[j * w + i]), cimagf(m[j * w + i]));
	}
      printf("\n");
    }
}

void transpose (fftw_complex *m, int w, int h)
{
  fftw_complex tmp;
  int i, j;

  for (j = 0; j < N; j++)
    {
      for (i = j + 1; i < N; i++)
	{
	  tmp = m[j * N + i];
	  m[j * N + i] = m[i * N + j];
	  m[i * N + j] = tmp;
	}
    }
}

void transpose_co (fftw_complex *m, int i0, int j0, int i1, int j1)
{
  fftw_complex tmp;
  int
    di = i1 - i0,
    dj = j1 - j0;

  if (di >= dj && di > LS)
    {
      transpose_co (m, i0, j0, (i0 + i1) / 2, j1);
      transpose_co (m, (i0 + i1) / 2, j0, i1, j1);
    }
  else if (dj > LS)
    {
      transpose_co (m, i0, j0, i1, (j0 + j1) / 2);
      transpose_co (m, i0, (j0 + j1) / 2, i1, j1);
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

	      tmp = m[i * N + j];
	      m[i * N + j] = m[j * N + i];
	      m[j * N + i] = tmp;
	    }
	}
    }
}

void transpose_co_par (fftw_complex *m, int i0, int j0, int i1, int j1)
{
  fftw_complex tmp;
  int
    di = i1 - i0,
    dj = j1 - j0;

  if (di >= dj && di > LS)
    {
      _Cilk_spawn transpose_co (m, i0, j0, (i0 + i1) / 2, j1);
      _Cilk_spawn transpose_co (m, (i0 + i1) / 2, j0, i1, j1);
    }
  else if (dj > LS)
    {
      _Cilk_spawn transpose_co (m, i0, j0, i1, (j0 + j1) / 2);
      _Cilk_spawn transpose_co (m, i0, (j0 + j1) / 2, i1, j1);
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

	      tmp = m[i * N + j];
	      m[i * N + j] = m[j * N + i];
	      m[j * N + i] = tmp;
	    }
	}
    }
}

int main (int argc, char **argv)
{
  fftw_complex mat1[N * N], mat2[N * N], twiddles[N];
  fftw_plan plan[N];
  int i, j;
  float tmp;

  populate (mat1, N * N);

  /* FFTW plan each row */
  for (i = 0; i < N; i++)
    plan[i] = fftw_plan_dft_1d (N, mat1 + i * N, mat2 + i * N,
				FFTW_FORWARD, FFTW_EXHAUSTIVE);

  populate (mat1, N * N);

  /* FFT on every row */
  _Cilk_for (i = 0; i < N; i++)
    fftw_execute (plan[i]);

  /* Multiplication by scalar */
  _Cilk_for (j = 0; j < N; j++)
    {
      for (i = 0; i < N; i++)
  	{
  	  tmp = -2.0 * M_PI * j * i / N;
	  mat2[j * N + i] *= (cos(tmp) + sin(tmp));
  	}
    }

  /* Transpose */
  /* transpose (mat2, N, N); */
  transpose_co (mat2, 0, 0, N, N);

  /* Cleanup old plans */
  for (i = 0; i < N; i++)
    fftw_destroy_plan(plan[i]);

  /* FFTW plan each row */
  for (i = 0; i < N; i++)
    plan[i] = fftw_plan_dft_1d (N, mat2 + i * N, mat1 + i * N,
				FFTW_FORWARD, FFTW_EXHAUSTIVE);

  /* FFT on every row again */
  _Cilk_for (i = 0; i < N; i++)
    fftw_execute (plan[i]);

  /* Transpose again */
  /* transpose (mat1, N, N); */
  transpose_co (mat1, 0, 0, N, N);

  print (mat1, N, N);

  /* Cleanup */
  for (i = 0; i < N; i++)
    fftw_destroy_plan(plan[i]);

  fftw_cleanup();

  return 0;
}
