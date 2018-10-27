#include <stdio.h>
#include <upc.h>

int main (int argc, char **argv)
{
  static int step = 10;
  int fah, cel;

  cel = step * MYTHREAD;
  fah = cel * (9.0 / 5.0) + 32;

  printf("%d \t %d \n", fah, cel);
  return 0;
}
