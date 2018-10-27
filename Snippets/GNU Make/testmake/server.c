#include <stdio.h>
#include "server_priv.h"
#include "server_access.h"

int main (int argc, char **argv)
{
  int i = server_priv_val();
  int j = server_access_val();

  printf("%d %d\n", i, j);
  return 0;
}
