#ifndef UTILITY_H
#define UTILITY_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <math.h>

/* benchmark functions */
#define BENCHMARK_MSG(func,runs,msg,ops)	\
  do {						\
    printf("run %-40s\t ", msg); 		\
    BENCHMARK(func,runs,ops)			\
      } while (0);				\

#define BENCHMARK_SPAWN(func,runs,msg,ops)				\
  do {									\
    int i;								\
    double meantime, *times = malloc(runs*sizeof(double));		\
    printf("run %-40s\t ", msg); 					\
    for (i = 0; i < runs; i++) { 					\
      starttime();							\
      spawn func; sync;							\
      times[i] = endtime();						\
    }									\
    meantime = calcMean(times, runs);					\
    printf("time: %.3fs\t Gflops: %.2f/%.2f\n", meantime, (ops/meantime)*1e-9, getMaxFlops()); \
    free(times);							\
  } while (0);								\

#define BENCHMARK(func,runs,ops)					\
  do {									\
    int i;								\
    double meantime, *times = malloc(runs*sizeof(double));		\
    for (i = 0; i < runs; i++) { 					\
      starttime();							\
      func;								\
      times[i] = endtime();						\
    }									\
    meantime = calcMean(times, runs);					\
    printf("time: %.5fs\t Gflops: %.5f/%.5f\n", meantime, (ops/meantime)*1e-9, getMaxFlops()); \
    free(times);							\
  } while (0);								\


/* private utility functions */
double calcMean(double *array, int size);
double getMaxFlops();

/* print */
void print_array(double** m, int dim);

/* memory alloc & dealloc function */
void free_array_slow(double** m, int dim);
void free_array(double **m);
void alloc_array_slow(double** *m, int dim);
void alloc_array(double** *m, int dim);
void init_array(double** m, int dim, int value);

#endif /* UTILITY_H */
