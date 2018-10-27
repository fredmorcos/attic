#include <upc_relaxed.h>
#include <stdio.h>

#define K 2

extern void cilk_test (int n);

typedef struct sq_mat {
  double blk[K][K];
} matblock_t;

shared matblock_t A[THREADS];

int main()
{
  int i,j;

  for (i = 0; i<K; i++) {
    for (j = 0; j<K; j++)
      A[MYTHREAD].blk[i][j] = i*K+j;
  }
    
  upc_barrier;

  upc_forall(i=0; i < THREADS; i++; &A[i]) {
    cilk_test(10);
  }
 
  return 0;	
}
