#include <math.h>
#include <stdio.h>

int main (int argc, char **argv)
{
  int i;

  upc_forall (i = 0; i < 1000000; i += 100000;)
    printf("%d %d\n", i, (i / 250000) % 4);

  return 0;
}
