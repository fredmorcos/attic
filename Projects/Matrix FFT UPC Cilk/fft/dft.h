#ifndef DFT
#define DFT

#include "cmplx.h"

void dft_twiddle_factors (complex *input,
			  complex *output,
			  complex *twiddles,
			  int size);

void dft_canonical (complex *input,
		    complex *output,
		    int size);

#endif
