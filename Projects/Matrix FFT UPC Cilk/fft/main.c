#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"
#include "dft.h"
#include "gettime.h"

void init_arrays (complex *input, complex *output, int size)
{
  int i;

  for (i = 0; i < size; i++)
    {
      input[i].real = (float) i;
      input[i].imag = 0.0;

      output[i].real = 0.0;
      output[i].imag = 0.0;
    }
}

void print_arrays (complex *input, complex *output, int size)
{
  int i;

  printf("input: ");
  for (i = 0; i < size; i++)
    complex_print (input[i]);
  printf("\n");

  printf("output: ");
  for (i = 0; i < size; i++)
    complex_print (output[i]);
  printf("\n");
}

int main (int argc, char **argv)
{
  int size;
  complex *input, *output, *twiddles;
  timespec_t start, end;

  if (argc < 2)
    {
      printf("./dft <size>\n");
      exit(1);
    }

  size     = atoi(argv[1]);
  input    = (complex *) malloc (sizeof(complex) * size);
  output   = (complex *) malloc (sizeof(complex) * size);
  twiddles = (complex *) malloc (sizeof(complex) * size);

  init_arrays (input, output, size);
  get_time_nsec (&start);
  dft_canonical (input, output, size);
  get_time_nsec (&end);
  print_arrays (input, output, size);
  printf("time: %.10f\n", time_diff_nsec (&start, &end));

  init_arrays (input, output, size);
  get_time_nsec (&start);
  dft_twiddle_factors (input, output, twiddles, size);
  get_time_nsec (&end);
  print_arrays (input, output, size);
  printf("time: %.10f\n", time_diff_nsec (&start, &end));

  free (input);
  free (output);

  return 0;
}
