#include <stdio.h>

int main (int argc, char **argv)
{
  /* should show '5.0' */
  printf("%3.1f\n", 5.0);
  /* should show '  5.0' */
  printf("%5.1f\n", 5.0);
  return 0;
}
