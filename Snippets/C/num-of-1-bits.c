#include <stdio.h>

int main (int argc, char **argv)
{
  int x = 10;
  int n = 0;

  while (x) {
    n += x & 1;
    x >>= 1;
  }

  printf("%d\n", n);

  return 0;
}
