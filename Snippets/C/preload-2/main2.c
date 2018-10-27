#include <stdio.h>

Cilk int fib (int n)
{
  if (n < 2) return n;
  else
    {
      int x, y;
      x = spawn fib (n-1);
      y = spawn fib (n-2);
      sync;
      return (x+y);
    }
}

int main (void) {
  int x = fib(10);
  printf("%d\n", x);
  return 0;
}
