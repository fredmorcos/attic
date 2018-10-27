#include "cmplx.h"

complex complex_mul (complex c1, complex c2)
{
  complex res;

  res.real = c1.real * c2.real - c1.imag * c2.imag;
  res.imag = c1.imag * c2.real + c1.real * c2.imag;

  return res;
}

complex complex_add (complex c1, complex c2)
{
  complex res;

  res.real = c1.real + c2.real;
  res.imag = c1.imag + c2.imag;

  return res;
}

void complex_print (complex c)
{
  printf("(%f + i%f) ", c.real, c.imag);
}
