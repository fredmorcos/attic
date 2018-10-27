#include <stdio.h>
#include <stdlib.h>
#include "bench.h"

#define LS 50

int table[LS];

int fibdyn(int n)
{
  if (n <= 2) return 1;
  if (n < LS)
    {
      if (table[n]) return table[n];
      table[n] = fibdyn(n - 2) + fibdyn(n - 1);
      return table[n];
    }
  else
    return fibdyn(n - 2) + fibdyn(n - 1);
}

int fib(int n)
{
  int a, b, i, s;

  a = 1;
  b = 1;

  s = 0;

  for (i = 3; i < n; i++)
    {
      s = a + b;

      if (i & 1)
	b = s;
      else
	a = s;
    }

  return a + b;
}

int fibrec(int n)
{
  if (n <= 2) return 1;
  return fibrec(n - 1) + fibrec(n - 2);
}

int main (int argc, char **argv)
{
  int n = atoi(argv[1]), res;
  double t;

  for (res = 0; res < LS; ++res)
    table[res] = 0;

  starttime();
  res = fib(n);
  t = endtime();
  printf("fib(%d) = %d, time = %f\n", n, res, t);

  starttime();
  res = fibdyn(n);
  t = endtime();
  printf("fibdyn(%d) = %d, time = %f\n", n, res, t);

  starttime();
  res = fibrec(n);
  t = endtime();
  printf("fibrec(%d) = %d, time = %f\n", n, res, t);

  return 0;
}
