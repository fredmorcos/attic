#include <stdio.h>

int g(int x) {
  return x + 5;
}

int (*f(int (*fp)(int)))(int) {
  return fp;
}

int main()
{
  int (*fp)(int) = f(g);
  printf("%d\n", fp(30));
  return 0;
}
