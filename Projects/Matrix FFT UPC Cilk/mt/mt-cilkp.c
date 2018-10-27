#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk_api.h>

#include "mt-cilkp.h"

/*cache oblivous recursive matrix transpose a-buf*/
void t_oopl_buf(int i0, int i1, int j0, int j1, int ldb, int ib, int jb)
{ 
  int di = i1 - i0, dj = j1 - j0;
  matblock_t *a = commons.a;
  double *buf = commons.buf;

  if (di >= dj && di > LS) {
    int im = (i0 + i1) / 2;
    _Cilk_spawn t_oopl_buf(i0, im, j0, j1, ldb, ib, jb);
    _Cilk_spawn t_oopl_buf(im, i1, j0, j1, ldb, ib, jb);

  } else if (dj > LS) {
    int jm = (j0 + j1) / 2;

    _Cilk_spawn t_oopl_buf(i0, i1, j0, jm, ldb, ib, jb );
    _Cilk_spawn t_oopl_buf(i0, i1, jm, j1, ldb, ib, jb );

  } else {
    int i,j;double t;
    for (i=i0;i<i1;i++)
      for (j=j0;j<j1;j++){
#ifndef OOPL
	t = a->blk[i][j];
#endif
	a->blk[i][j] = buf[(j-jb)*ldb+(i-ib)];
#ifndef OOPL
	buf[(j-jb)*ldb+(i-ib)] = t;
#endif
      }
  }
}

/*cache oblivious recursive out-of-place swap inside matrix a: top right blk vs bottom left */
void t_oopl( unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1){
  unsigned int di = i1 - i0, dj = j1 - j0;
  matblock_t *a = commons.a;

  if (di >= dj && di > LS) {
    unsigned int im = (i0 + i1) / 2;
    _Cilk_spawn t_oopl(i0, im, j0, j1);
    _Cilk_spawn t_oopl(im, i1, j0, j1);
  } else if (dj > LS) {
    unsigned int jm = (j0 + j1) / 2;
    _Cilk_spawn t_oopl(i0, i1, j0, jm);
    _Cilk_spawn t_oopl(i0, i1, jm, j1);
  } else {
    unsigned int i,j;double t;
    for (i = i0; i < i1; i++){
      for (j = j0; j < j1; j++){
	t = a->blk[i][j];
	a->blk[i][j] = a->blk[j][i];
	a->blk[j][i] = t;
      }
    }
  }
}

/*cache oblivious recursive in-place swap inside matrix a */
void t_inpl(unsigned int k0, unsigned int k1){ 
  matblock_t *a = commons.a;
  if (k1-k0 > LS) {
    int km =  (k0+k1) / 2;
    _Cilk_spawn t_inpl( k0, km );   //upper left
    _Cilk_spawn t_inpl( km, k1 ); //lower right
    _Cilk_spawn t_oopl( k0, km , km, k1 );

  } else {
    unsigned int i,j;double t;
    for (i=k0; i < k1; i++){
      for (j=i+1; j < k1; j++){
	t = a->blk[i][j];
	a->blk[i][j] = a->blk[j][i];
	a->blk[j][i] = t;
      }
    }
  }
}

void i_block(unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1){ 
  matblock_t *a = commons.a;
  int di = i1 - i0; int dj = j1 - j0;
  if (di >= dj && di > LS) {
    unsigned int im = (i0 + i1) / 2;
    _Cilk_spawn i_block(I, J, i0, im, j0, j1);
    _Cilk_spawn i_block(I, J, im, i1, j0, j1);
  } else if (dj > LS) {
    unsigned int jm = (j0 + j1) / 2;
    _Cilk_spawn i_block(I, J, i0, i1, j0, jm);
    _Cilk_spawn i_block(I, J, i0, i1, jm, j1);
  
  } else {
    unsigned int i,j;
    for (i=i0; i < i1; i++){
      for (j=j0; j < j1; j++){
	a->blk[i][j] =  ((double)I*K+i)*(double)N +(double)J*K+j;
      }
    }
  }
}

int c_block(int trans, unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1){ 
  matblock_t *a = commons.a;
  int di = i1 - i0; int dj = j1 - j0;
  int e1, e2;

  if (di >= dj && di > LS) {
    unsigned int im = (i0 + i1) / 2;
    e1 = _Cilk_spawn c_block(trans, I, J, i0, im, j0, j1);
    e2 = _Cilk_spawn c_block(trans, I, J, im, i1, j0, j1);
    _Cilk_sync;
    return e1 + e2;
  } else if (dj > LS) {
    unsigned int jm = (j0 + j1) / 2;
    e1 = _Cilk_spawn c_block(trans, I, J, i0, i1, j0, jm);
    e2 = _Cilk_spawn c_block(trans, I, J, i0, i1, jm, j1);
    _Cilk_sync;
    return e1+e2;
  } else {
    unsigned int i,j, err=0; double val;
    for (i=i0; i < i1; i++){
      for (j=j0; j < j1; j++){
	val = (trans==0 ? ((double)I*K+i)*N+((double)J*K+j) : ((double)J*K+j)*N+((double)I*K+i));
	if (a->blk[i][j] != val){
	  if (N <16)
	    printf("%d,%d: %d-%d %g != %g\n",I,J,i,j,val ,a->blk[i][j] );
	  err++;
	}
      }
    }
    return err;
  }
}
