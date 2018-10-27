void addvecs (double *veca, double *vecb, double *vecc, int i0, int i1)
{
  int i;

  for (i = i0; i < i1; i++)
    vecc[i] = veca[i] + vecb[i];
}
