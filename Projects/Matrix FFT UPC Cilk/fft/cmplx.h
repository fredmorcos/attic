#ifndef CMPLX
#define CMPLX

typedef struct _complex
{
  float real, imag;
} complex;

complex complex_mul (complex c1, complex c2);
complex complex_add (complex c1, complex c2);
void complex_print (complex c);

#endif
