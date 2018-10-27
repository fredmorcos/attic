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

/* void _dft_ct_dit (complex *input, */
/* 		  complex *output, */
/* 		  complex *twiddles, */
/* 		  int start, */
/* 		  int size) */
/* { */
/* } */

/* void dft_ct_dit (complex *input, */
/* 		 complex *output, */
/* 		 complex *twiddles, */
/* 		 int size) */
/* { */
/*   _dft_ct_dit (input, output, twiddles, 0, size); */
/* } */

/* void dft_split (complex *input, */
/* 		complex *output, */
/* 		complex *twiddles, */
/* 		int size) */
/* { */
/*   int k, m; */
/*   complex tmp1, tmp2; */

/*   _calc_twiddle_factors (twiddles, size); */

/*   for (k = 0; k < size; k++) */
/*     for (n = 0; n < (size / 2) - 1; n++) */
/*       tmp1 = complex_add (tmp1, input */
/* } */

void dft_canonical (complex *input,
		    complex *output,
		    int size)
{
  int k, n;
  float tmp;
  complex tmp_complex;

  for (k = 0; k < size; k++)
    for (n = 0; n < size; n++)
      {
	tmp = -2.0 * M_PI * k * n / size;
	tmp_complex.real = cos(tmp);
	tmp_complex.imag = sin(tmp);
	output[k] = complex_add (output[k], complex_mul (input[n],
							 tmp_complex));
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
