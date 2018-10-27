#include <stdio.h>
#include <stdlib.h>

#define BUPC_USE_UPC_NAMESPACE

#include <upc_relaxed.h>
#include <upc_collective.h>
#include <bupc_timers.h>

#include "params.h"
#include "cilk+-transpose.h"

#define second() (TIME()/1000000.0)

shared matblock_t A[THREADS];

upc_cilk_common_t commons;

double bufA[BSIZE];
double bufB[BSIZE];

/* *********************************************************************** */
/* setup initial data with consecutive row major numbering                 */
/* *********************************************************************** */
void initM (shared matblock_t *X)
{
  int b,i,j,ki,kj, I, J;
  upc_forall(b=0; b < THREADS; b++; &A[b]){
    I = (MYTHREAD / PSQRT) ;
    J = (MYTHREAD % PSQRT) ;
    ki = (I == PSQRT -1)?  N-K*(PSQRT-1): K;
    kj = (J == PSQRT -1)?  N-K*(PSQRT-1): K;
	
    i_block(I,J,0,ki,0,kj);	

  }
}


/*recursivly copys fitting blocks into buf from the adjacent transpose block in matrix A[bt]*/
/*and applies cache oblivious transpose between the subblock of A[MYTHREAD] and buf*/

void t_oopl_cbuf(int i0, int i1, int j0, int j1, int bt)
{ 
  int di = i1 - i0, dj = j1 - j0, i, j;
  double *b = commons.buf;
  matblock_t *a = commons.a;
  shared matblock_t *at = &A[bt];
  void * dstA[1];
  void * dstB[1];
  upc_handle_t a_handle,b_handle;
  /*re-check if this really is the max?*/
  shared const void * srclst[K];
  double sec1,sec2,sec3;

  if ( di*dj <= 2*BSIZE) {

    int jm = (j0 + j1) / 2;
    for (j=j0; j < j1; j++) {  // block into buffer 
      srclst[j-j0] = &(at->blk[j][i0]);
    }
    dstA[0] = bufA;
    dstB[0] = bufB;

    upc_memget_ilist(1,dstA,di*(jm-j0)*sizeof(double), (jm-j0), srclst, di*sizeof(double)); //fetchB first half into BufA

    b_handle=upc_memget_ilist_async(1,dstB,di*(j1-jm)*sizeof(double), (j1-jm), &(srclst[jm-j0]), di*sizeof(double)); //fetchNB second half into BuFB

    commons.buf = bufA;
#ifndef COMONLY
    t_oopl_buf( i0,i1,j0,jm,di,i0, j0 ); //transpose first local half with BufA  
#endif

    upc_waitsync(b_handle);            //sync
    a_handle=upc_memput_ilist_async(jm-j0,(void * const)srclst,di*sizeof(double),1,(const void * const) dstA,di*(jm-j0)*sizeof(double)); //putNB BufA back

    commons.buf = bufB;
#ifndef COMONLY
    t_oopl_buf( i0,i1,jm,j1, di,i0, jm );  //transpose second local half with BufB
#endif

    upc_memput_ilist(j1-jm,(void *const) &(srclst[jm-j0]),di*sizeof(double),1,(const void * const)dstB,di*(j1-jm)*sizeof(double));   //putB BufB back   
    upc_waitsync(a_handle);

  
  } else if (di > dj ) {
    int im = (i0 + i1) / 2;
    t_oopl_cbuf(i0, im, j0, j1, bt );
    t_oopl_cbuf(im, i1, j0, j1, bt );

  } else  { // dj >= di
    int jm = (j0 + j1) / 2;
    t_oopl_cbuf(i0, i1, j0, jm, bt );
    t_oopl_cbuf(i0, i1, jm, j1, bt );
  } 
}


void t_blockedP5 (shared struct sq_mat *A){
  int b, I, J, i, j,k,k2;
  double t; 

  upc_forall (b = 0; b < THREADS; b++; &A[b]){
    I = b / PSQRT  ;    //global block indices
    J = b % PSQRT  ;
    k = (I == PSQRT -1)?  N-K*(PSQRT-1): K;
    if ( I == J ){
#ifndef COMONLY
      t_inpl(0,k);
#endif
    }else if (I < J){  //work on top half row blocks of north-east
      int bt=J*PSQRT + I; 
      t_oopl_cbuf( 0, K2, 0, k, bt ); 
			  
    }else {  // right half column block of southwest (=eastsouthwest)
      int bt = J*PSQRT + I; int bs;
      t_oopl_cbuf( 0 ,k, K2, K, bt ); //offset, target block
    }
  }
}



shared int err[THREADS];
/* trans = 0 -> check original mat */
void checkm(shared matblock_t *A, int trans){

  int b, i, j, ki, kj, I, J; double val;

  upc_forall(b=0; b < THREADS; b++; &A[b]){
    I = (b / PSQRT) ;    //global indices of block local values
    J = (b % PSQRT) ;
    ki = (I == PSQRT -1)?  N-K*(PSQRT-1): K;
    kj = (J == PSQRT -1)?  N-K*(PSQRT-1): K;
    err[b] = c_block(trans, I, J, 0, ki, 0, kj); 
  }   
}

void printm(shared matblock_t *A, const char* frmt) {
  
  int block,I,J;

  for (I=0; I < N; I++){
    for (J=0; J < N; J++){
      block = (I / K) *PSQRT + J / K;
      printf(frmt,A[block].blk[I % K][J % K]);
    }
    printf("\n"); 
  }
}

void printa(shared matblock_t *A, const char* frmt) {

  int block,I,J;

  for (I=0; I < N; I++){
    for (J=0; J < N; J++){
      block = (I / K) *PSQRT + J/K;
      printf(frmt,upc_threadof(&A[block].blk[I % K][J % K]));
    }
    printf("\n"); 
  }
}

int main(int argc, char* argv[]) {

  int i,terr, ct;
  double sec,sec1,gswp,gcom;
  setenv("CILK_NWORKERS", bupc_getenv("UPC_CILK_NWORKERS"), 1);


  if (THREADS != PSQRT*PSQRT){
    printf("the number of threads does not fit the data partitioning: %d != %d x %d\n",THREADS,PSQRT,PSQRT);
    upc_global_exit(1);
  }

  commons.a = (matblock_t*)&A[MYTHREAD];
  commons.buf = bufA;

#ifndef COMONLY
  initM(A);
#endif

  upc_barrier;
  t_blockedP5(A);
  upc_barrier;  

  sec = second();
  t_blockedP5(A);
  upc_barrier;  
  sec1 = second()-sec;
#ifndef COMONLY
  checkm(A,0);
  upc_barrier;
#endif
  if (!MYTHREAD) {
    gswp =  1e-9 /sec1 * N * N * sizeof(double);
    gcom =  1e-9 / sec1 * ((double)N * N  - (double)N*N/PSQRT)*sizeof(double);
    printf("mt-2cbuf-cilk+: N=%d K=%d BUFSIZE=%d %d UThreads %d CThreads , rt %f %f GBswp/s %f GBcom/s ",N, K, BSIZE, THREADS,__cilkrts_get_nworkers() ,sec1,gswp,gcom);
    terr=0;

#ifndef COMONLY
    for (i=0; i < THREADS; i++)
      terr += err[i];
    if (terr==0)
      fprintf(stdout, " ..success\n");
    else
      fprintf(stdout, " ..failed with %d errors\n",terr);
#else
      fprintf(stdout, " ..comm only - unchecked\n");
#endif

    if (N < 16){
      printm(A, "%02.0f ");
    }

  }
  
  upc_barrier;
}  
