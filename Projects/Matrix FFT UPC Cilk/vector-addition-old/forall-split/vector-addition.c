void add_vectors(float *v1, float *v2, float *v3, int i0, int i1)
{
  int i;

  /* for (i = i0; i < i1; i++) */
  for (i = 0; i < i1 - i0; i++)
    v3[i] = v1[i] + v2[i];
}
