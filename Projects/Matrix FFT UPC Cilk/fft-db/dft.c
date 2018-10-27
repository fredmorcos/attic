#include "dft.h"
#include <math.h>
#include <complex.h>

void _calc_twiddle_factors (float complex *twiddles, int size)
{
  int k;
  float tmp;

  /*
   * Calculate the twiddle factors, we need only N of them
   * due to the periodic nature of trigonometric functions
   * where e^(xi) = cos x + i sin x
   */
  for (k = 0; k < size; k++)
    {
      tmp = -2.0 * M_PI * k / size;
      twiddles[k] = cos(tmp) + sin(tmp) * I;
    }
}

void dft_twiddle_factors (float complex *input,
			  float complex *output,
			  float complex *twiddles,
			  int size)
{
  int k, n;

  _calc_twiddle_factors (twiddles, size);

  for (k = 0; k < size; k++)
    for (n = 0; n < size; n++)
      output[k] += input[n] * twiddles[n * k % size];
	/* output[k] = complex_add (output[k],  */
	/* 			 complex_mul (input[n], */
	/* 				      twiddles[n * k % size])); */
}
