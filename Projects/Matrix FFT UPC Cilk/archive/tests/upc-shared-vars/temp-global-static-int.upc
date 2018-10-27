#include <stdio.h>
#include <upc.h>

static int step = 10;

int main (int argc, char **argv)
{
  int fah, cel;

  cel = step * MYTHREAD;
  fah = cel * (9.0 / 5.0) + 32;

  printf("%d \t %d \n", fah, cel);
  return 0;
}
