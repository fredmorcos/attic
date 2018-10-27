#include <upc_relaxed.h>
#define N 100*THREADS

shared int v1[N], v2[N], v1pv2[N];

int
main (int argc, char **argv)
{
  int i = 0;

  upc_forall (i = 0; i < N; i++; i)
    {
	  v1pv2[N] = v1[i] + v2[i];
    }

  return 0;
}
