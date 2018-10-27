#ifndef DFT
#define DFT

#include <complex.h>

void dft_twiddle_factors (float complex *input,
			  float complex *output,
			  float complex *twiddles,
			  int size);

#endif
