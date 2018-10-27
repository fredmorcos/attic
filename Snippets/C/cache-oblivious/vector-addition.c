#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SIZE 300000000
#define LS   2000

void vecadd (double *arr1, double *arr2, int l, int r)
{
  int d = r - l;
  
  if (d <= LS) {
    int i;

    for (i = l; i < r; i++)
      arr1[i] *= arr2[i];
  } else {
    int m = l + (d / 2);

    vecadd (arr1, arr2, l, m);
    vecadd (arr1, arr2, m, r);
  }
}

int main (int argc, char **argv)
{
  clock_t start, end;

  double *arr1 = (double *) malloc ((sizeof (double)) * SIZE);
  double *arr2 = (double *) malloc ((sizeof (double)) * SIZE);

  long i;

  if (!arr1 || !arr2) {
    printf("cannot allocate\n");
    exit(1);
  }

  for (i = 0; i < SIZE; i++) {
    arr1[i] = (double) i;
    arr2[i] = (double) i;
  }

  start = clock();

  /* for (i = 0; i < SIZE; i++) */
  /*   arr1[i] *= arr2[i]; */

  vecadd (arr1, arr2, 0, SIZE);

  end = clock();

  printf("time %fs\n", ((double) end - start) / CLOCKS_PER_SEC);

  /* for (i = 0; i < 5; i++) */
  /*   printf("%f ", arr1[i]); */

  /* printf("\n"); */

  /* for (i = SIZE - 1; i >= SIZE - 5; i--) */
  /*   printf("%f ", arr1[i]); */

  /* printf("\n"); */

  free(arr1);
  free(arr2);
  return 0;
}
