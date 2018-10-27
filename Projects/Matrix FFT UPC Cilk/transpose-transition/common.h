#ifndef __COMMON_H__
#define __COMMON_H__

/* matrix info */
#define N MAT_WIDTH
#define M MAT_HEIGHT

/* thread info */
#define T  MYTHREAD
#define NT THREADS

/* blocking and leaf size info */
#define BS  N/NT
#define ILS LEAF_SIZE_I
#define JLS LEAF_SIZE_J

#define RUNS 1

#endif /* __COMMON_H__ */
