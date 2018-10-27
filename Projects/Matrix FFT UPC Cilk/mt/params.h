#ifndef PARAMS_H
#define PARAMS_H


#ifndef PSQRT
#define PSQRT 1
#endif

/*#define P (PSQRT*PSQRT) */

/*global matrix size per dimension*/

#ifndef MATN
#define N 65536
#else
#define N MATN
#endif

/*size of blocked matrix*/
#ifndef K
#define K ((N+PSQRT-1)/PSQRT)
#endif

#ifndef LS
#define LS 8
#endif

#define K2 (K>>1)

typedef struct sq_mat {
  double blk[K][K];
}matblock_t;

#ifndef PGSIZEDBL
#define PGSIZEDBL 512
#endif

/* COPY BUFFER SIZE */
#ifndef BUFFER_SIZE
#define BUFFER_SIZE (16000*16000)
#endif


#endif //PARAMS_H
