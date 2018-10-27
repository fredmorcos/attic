#include <stdio.h>
#include <time.h>

int main (int argc, char **argv)
{
  printf("%ld %ld %ld %ld\n", sizeof(int), sizeof(long), sizeof(long long), sizeof(time_t));

  return 0;
}
