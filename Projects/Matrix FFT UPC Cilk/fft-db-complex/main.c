#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include "dft.h"
#include "gettime.h"

void populate (float complex *m, int size)
{
  int i;

  for (i = 0; i < size; i++)
    m[i] = ((float) i) + 0.0 * I;
}

void print (float complex *m, int w, int h)
{
  int i, j;

  for (j = 0; j < h; j++)
    {
      for (i = 0; i < w; i++)
	{
	  printf("(%3.5f + i%.5f) ", crealf(m[j * w + i]), cimagf(m[j * w + i]));
	}
      printf("\n");
    }
}

int main (int argc, char **argv)
{
  float complex mat1[N * N], mat2[N * N], twiddles[N];
  int i, j;
  float tmp;

  populate (mat1, N * N);
  print (mat1, N, N);

  /* FFT on every row */
  for (i = 0; i < N; i++)
    {
      dft_twiddle_factors (mat1 + i * N,
  			   mat2 + i * N,
  			   twiddles,
  			   N);
    }

  /* Multiplication by scalar */
  for (j = 0; j < N; j++)
    {
      for (i = 0; i < N; i++)
  	{
  	  tmp = -2.0 * j * i / N;
	  mat2[j * N + i] = cos(tmp) + sin(tmp) * I;
  	}
    }

  /* Transpose */
  for (j = 0; j < N; j++)
    {
      for (i = j + 1; i < N; i++)
	{
	  tmp  = mat2[j * N + i];
	  mat2[j * N + i] = mat2[i * N + j];
	  mat2[i * N + j] = tmp;
	}
    }

  /* FFT on every row again */
  for (i = 0; i < N; i++)
    {
      dft_twiddle_factors (mat2 + i * N,
  			   mat1 + i * N,
  			   twiddles,
  			   N);
    }

  printf ("\n\n");
  print (mat1, N, N);

  return 0;
}
