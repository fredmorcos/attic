#ifndef DFT
#define DFT

#include "cmplx.h"

void dft_twiddle_factors (complex *input,
			  complex *output,
			  complex *twiddles,
			  int size);

#endif
