#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"
#include "dft.h"
#include "gettime.h"

void populate (complex *m, int size)
{
  int i;

  for (i = 0; i < size; i++)
    {
      m[i].real = (float) i;
      m[i].imag = 0.0;
    }
}

void print (complex *m, int w, int h)
{
  int i, j;

  for (j = 0; j < h; j++)
    {
      for (i = 0; i < w; i++)
	{
	  printf("(%3.5f + i%.5f) ", m[j * w + i].real, m[j * w + i].imag);
	}
      printf("\n");
    }
}

int main (int argc, char **argv)
{
  complex mat1[N * N], mat2[N * N], twiddles[N];
  int i, j;
  float tmp, tmp2;

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
  	  mat2[j * N + i].real = cos(tmp);
  	  mat2[j * N + i].imag = sin(tmp);
  	}
    }

  /* Transpose */
  for (j = 0; j < N; j++)
    {
      for (i = j + 1; i < N; i++)
	{
	  tmp  = mat2[j * N + i].real;
	  tmp2 = mat2[j * N + i].imag;
	  
	  mat2[j * N + i].real = mat2[i * N + j].real;
	  mat2[j * N + i].imag = mat2[i * N + j].imag;

	  mat2[i * N + j].real = tmp;
	  mat2[i * N + j].imag = tmp2;
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
