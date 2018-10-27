#include <stdio.h>
#include <stdlib.h>

#define SIZE 400000000

void populate (double *ll, int l, int r)
{
  int d = r - l;

  if (d <= 2560) {
    int i;

    for (i = l; i < r; i++)
      ll[i] = (double) i;
  } else {
    populate (ll, l, l + (d / 2));
    populate (ll, l + (d / 2), r);
  }
}

int main (int argc, char **argv)
{
  double *l1 = (double *) malloc ((sizeof (double)) * SIZE);
  int i;

  if (!l1) {
    printf("cannot allocate\n");
    exit(1);
  }

  for (i = 0; i < SIZE; i++)
    l1[i] = i;

  /* populate (l1, 0, SIZE); */

  /* for (i = 0; i < 10; i++) */
  /*   printf("%d ", l1[i]); */

  /* printf("\n"); */

  /* for (i = SIZE - 1; i >= SIZE - 10; i--) */
  /*   printf("%d ", l1[i]); */

  /* printf("\n"); */

  free(l1);
  return 0;
}
