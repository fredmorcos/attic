#define N MAT_WIDTH

int transpose(float **m, int width)
{
  for (i = 0; i < N/2; i++) {
    for (j = 0; j < width; j++) {
      tmp     = m[i][j];
      m[i][j] = m[j][i];
      m[j][i] = tmp;
    }
  }
}
