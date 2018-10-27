#include "dft.h"
#include "cmplx.h"
#include <math.h>

void _calc_twiddle_factors (complex *twiddles, int size)
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
      twiddles[k].real = cos(tmp);
      twiddles[k].imag = sin(tmp);
    }
}

void dft_twiddle_factors (complex *input,
			  complex *output,
			  complex *twiddles,
			  int size)
{
  int k, n;

  _calc_twiddle_factors (twiddles, size);

  for (k = 0; k < size; k++)
    for (n = 0; n < size; n++)
	output[k] = complex_add (output[k], 
				 complex_mul (input[n],
					      twiddles[n * k % size]));
}
