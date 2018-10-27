#include <stdio.h>
#include <math.h>

void
half (int n)
{
  int q = 0, i;

  printf("%d/2\n", n);

  while (n >= 2)
    {
      for (i = 1; ((int) pow(2, i)) <= n; i++);
      printf("i = %d, d = %d\n", i - 1, (int) pow(2, i - 1));
      n -= (int) pow(2, i - 1);
      q += i;
    }

  printf("quotient = %d, remainder = %d\n", q, n);
}

int
main (int argc, char **argv)
{
  half(31);
  return 0;
}
