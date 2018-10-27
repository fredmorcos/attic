#include <stdio.h>
#include "client_api.h"
#include "client_mem.h"

int main (int argc, char **argv)
{
  int i = client_api_val();
  int j = client_mem_val();

  printf("%d %d\n", i, j);
  return 0;
}
