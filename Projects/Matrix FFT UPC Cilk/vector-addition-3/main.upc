#include <stdio.h>
#include <math.h>
#include "gettime.h"

#define BUPC_USE_UPC_NAMESPACE
#include <upc.h>
#include <bupc_extensions.h>

#define N  VEC_LEN
#define T  MYTHREAD
#define NT THREADS
/* #define B  BS */

const int B=BS;

#define MIN(a,b) (a < b ? a : b)

/* note: (index / B) % NT = MYTHREAD */

/* source and destination vectors */
shared [B] double veca[N];
shared [B] double vecb[N];
shared [B] double vecc[N];

/* per thread-specific stuff */
shared double time_diffs[NT];

extern void addvecs (double *veca, double *vecb, double *vecc, int i0, int i1);

void print_vec (shared [B] double *vec)
{
#ifdef SHOW_VEC
  int i;

  for (i = 0; i < N; i++)
    printf("%5.1f ", vec[i]);
  printf("\n");

  for (i = 0; i < N; i++)
    printf("%5d ", (int) upc_threadof(&vec[i]));
  printf("\n");
#endif /* SHOW_VEC */
}

int thread_id (int i)
{
  return (i / B);
}

int main (int argc, char **argv)
{
  timespec_t start, end;
  int
    i,      /* vector elements or "partition" iterator */
    j,      /* vector "partition elements" iterator */
    thread, /* threads iterator */
    run;    /* number of runs iterator */

  /* fill and print the source vectors */
  if (T == 0) {
    for (i = 0; i < N; i++)
      veca[i] = vecb[i] = (double) i;

    print_vec(veca);
    print_vec(vecb);
  }

  upc_barrier;

  for (run = 0; run < RUNS; run++) {
    get_time_nsec (&start);

#if defined (SINGLE_THREAD)
    if (T == 0)
      for (i = 0; i < N; i++)
	vecc[i] = veca[i] + vecb[i];
/* #elif defined (SINGLE_THREAD_FORALL) */
/*     upc_forall (i = 0; i < N; i++; 1,2,3,0) */
/*     /\* upc_forall (i = 0; i < N; i++; thread_id (i)) *\/ */
/*       vecc[i] = veca[i] + vecb[i]; */
#elif defined (LOCAL_BLOCKS)
#define LB (N / NT)
    /* 0, (T * B) + B < N ? B : N - (T * B)); */
    /* 0, (int) ceil((double) N / NT)); */
    /* 0, (N / NT) + (T == 0 ? N % NT : 0)); */
    addvecs ((double *) veca, (double *) vecb, (double *) vecc, 0, LB);
#elif defined (FORALL_AFFINITY)
    upc_forall (i = 0; i < N; i++; &veca[i])
      vecc[i] = veca[i] + vecb[i];
#elif defined (FORALL_AFFINITY_SPLIT)
    upc_forall (i = 0; i < N; i += B; &veca[i])
      addvecs ((double *) &veca[i], (double *) &vecb[i], (double *) &vecc[i], 0, B);
#elif defined (FORALL_ITERATION)
    upc_forall (i = 0; i < N; i++; i)
      vecc[i] = veca[i] + vecb[i];
#elif defined (FORALL_ITERATION_SPLIT)
    /* this only makes sense for B=1 */
    upc_forall (i = 0; i < N; i += B; i)
      addvecs ((double *) &veca[i], (double *) &vecb[i], (double *) &vecc[i], 0, B);
#elif defined (FORALL_PARTITIONED)
    upc_forall (i = 0; i < N; i++; i / B)
      vecc[i] = veca[i] + vecb[i];
#elif defined (FORALL_PARTITIONED_SPLIT)
    upc_forall (i = 0; i < N; i += B; i / B)
      addvecs ((double *) &veca[i], (double *) &vecb[i], (double *) &vecc[i], 0, B);
#elif defined (FOR_SINGLE_ELEMENT_BLOCKS)
    /* works well with B=1, slow with B=N/NT remote accesses. */
    for (i = T; i < N; i += NT)
      vecc[i] = veca[i] + vecb[i];
#elif defined (FOR_BLOCKS_TRAVERSAL)
    for (i = T * B; i < N; i += NT * B)
      for (j = i; j < i + B; j++)
	vecc[j] = veca[j] + vecb[j];
#elif defined (FOR_BLOCKS_TRAVERSAL_SPLIT)
    for (i = T * B; i < N; i += NT * B)
      addvecs ((double *) &veca[i], (double *) &vecb[i], (double *) &vecc[i], 0, B);
#elif defined (FOR_PARTITIONED)
    /* will not work with B=1 since it depends on large partitioning blocks. */
    for (i = T * B; i < MIN(B * (T + 1), N); i++)
      vecc[i] = veca[i] + vecb[i];
#elif defined (FOR_PARTITIONED_SPLIT)
    /* will not work with B=1 since it depends on large partitioning blocks. */
    for (i = T * B; i < MIN(B * (T + 1), N); i += B)
      addvecs ((double *) &veca[i], (double *) &vecb[i], (double *) &vecc[i], 0, B);
#elif defined (SINGLE_THREAD_BULK_COPY)
#define BLOCK_LEN (sizeof (double) / sizeof (char))
    if (T == 0) {
      double vecblock_a[B], vecblock_b[B], vecblock_c[B];

      for (i = 0; i < N; i += B) {
	upc_memget (vecblock_a, &veca[i], B * BLOCK_LEN);
	upc_memget (vecblock_b, &vecb[i], B * BLOCK_LEN);

	for (j = 0; j < B; j++)
	  vecblock_c[j] = vecblock_a[j] + vecblock_b[j];

	upc_memput (&vecc[i], vecblock_c, B * BLOCK_LEN);
      }
    }
#elif defined (SINGLE_THREAD_BULK_COPY_SPLIT)
#define BLOCK_LEN (sizeof (double) / sizeof (char))
    if (T == 0) {
      double vecblock_a[B], vecblock_b[B], vecblock_c[B];

      for (i = 0; i < N; i += B) {
	upc_memget (vecblock_a, &veca[i], B * BLOCK_LEN);
	upc_memget (vecblock_b, &vecb[i], B * BLOCK_LEN);

	addvecs (vecblock_a, vecblock_b, vecblock_c, 0, B);

	upc_memput (&vecc[i], vecblock_c, B * BLOCK_LEN);
      }
    }
#elif defined (SINGLE_THREAD_BULK_COPY_ASYNC)
#define BLOCK_LEN (sizeof (double) / sizeof (char))
    if (T == 0) {
      double vecblock_a[B], vecblock_b[B], vecblock_c[B];
      upc_handle_t handle_a, handle_b;

      for (i = 0; i < N; i += B) {
	handle_a = upc_memget_async (vecblock_a, &veca[i], B * BLOCK_LEN);
	handle_b = upc_memget_async (vecblock_b, &vecb[i], B * BLOCK_LEN);

	upc_waitsync(handle_a);
	upc_waitsync(handle_b);

	for (j = 0; j < B; j++)
	  vecblock_c[j] = vecblock_a[j] + vecblock_b[j];

	upc_memput (&vecc[i], vecblock_c, B * BLOCK_LEN);
      }
    }
#elif defined (SINGLE_THREAD_BULK_COPY_ASYNC_SPLIT)
#define BLOCK_LEN (sizeof (double) / sizeof (char))
    if (T == 0) {
      double vecblock_a[B], vecblock_b[B], vecblock_c[B];
      upc_handle_t handle_a, handle_b;

      for (i = 0; i < N; i += B) {
	handle_a = upc_memget_async (vecblock_a, &veca[i], B * BLOCK_LEN);
	handle_b = upc_memget_async (vecblock_b, &vecb[i], B * BLOCK_LEN);

	upc_waitsync(handle_a);
	upc_waitsync(handle_b);

	addvecs (vecblock_a, vecblock_b, vecblock_c, 0, B);

	upc_memput (&vecc[i], vecblock_c, B * BLOCK_LEN);
      }
    }
#else /* METHOD */
    printf("No method specified.\n");
#endif /* METHOD */

    get_time_nsec (&end);
    time_diffs[T] = time_diff_nsec (&start, &end);
    upc_barrier;
  }

  upc_barrier;

  if (T == 0) {
    print_vec (vecc);

#ifdef DO_CHECK
    for (i = 0; i < N; i++)
      if (vecc[i] != (2.0 * (double) i)) {
	printf("error\n");
	goto leave;
      }
    printf("success\n");
  leave:
#endif /* DO_CHECK */
  }

  if (T == 0)
    for (thread = 0; thread < NT; thread++)
      printf ("T %2d: %.15f s\n", thread, time_diffs[thread]);

  return 0;
}
