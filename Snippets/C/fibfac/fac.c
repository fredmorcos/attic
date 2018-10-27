#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long int f;

f fac(int n)
{
  f res = n--;

  while (n > 1)
    {
      res *= n--;
    }

  return res;
}

f facrec(int n)
{
  if (n == 2) return 2;
  return n * facrec(n - 1);
}

int main (int argc, char **argv)
{
  int n = atoi(argv[1]);
  printf("fac(%d) = %llu\n", n, fac(n));
  printf("facrec(%d) = %llu\n", n, facrec(n));
  return 0;
}
