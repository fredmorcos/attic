#ifndef UPC_CILKPT_H
#define UPC_CILKPT_H

#include "params.h"

typedef struct u_c_c{
  /*pointer to a thread private buffer */
  double *buf;
  /*pointer to upc local=private portion of shared A*/
  matblock_t *a;
} upc_cilk_common_t;

extern upc_cilk_common_t commons;


/* Interface seen by g++ for linking with UPC*/
#ifdef __cplusplus

extern "C" void t_oopl_buf( int i0, int i1, int j0, int j1, int ldb, int ib, int jb);
extern "C" void t_oopl( int i0, int i1, int j0, int j1);
extern "C" void t_inpl( int k0, int k1);
extern "C" void i_block(unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1);
extern "C"  int c_block(int trans, unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1);

#endif

/* Interface seen by UPC */
#ifdef __UPC__
 
extern void t_oopl_buf( int i0, int i1, int j0, int j1, int ldb, int ib, int jb);
extern void t_oopl( int i0, int i1, int j0, int j1);
extern void t_inpl( int k0, int k1);
extern void i_block(unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1);
extern int  c_block(int trans, unsigned int I, unsigned int J, unsigned int i0, unsigned int i1, unsigned int j0, unsigned int j1);
 
#endif /* __UPC__ */

#endif /* UPC_CILKPT_H */
