#include <stdio.h>
#include "gettime.h"

#define BUPC_USE_UPC_NAMESPACE
#include <upc.h>
#include <bupc_extensions.h>

#define MAX(a,b) (a >= b ? a : b)
#define MIN(a,b) (a <  b ? a : b)

#define T  MYTHREAD
#define NT THREADS

#define UPC_ILS MIN(GILS, BS)
#define UPC_JLS MIN(GJLS, BS)

/*
   this is a function used to traverse a 2-dimensional matrix
   in a two-fold loop fashion. h is the height, w the width,
   f1 is the function to be executed inside the j loop (width),
   f2 is the function to be executed inside the i loop (height),
   del is a delimiter that is printed once before the
   traversal starts (before the i loop), and once after it is
   finished (after the i loop).
*/
#define MAT_DO(h,w,f1,f2,del)			\
  do {						\
    int i, j;					\
    printf(del);				\
    for (i = 0; i < h; i++) {			\
      for (j = 0; j < w; j++) {			\
	f1;					\
      }						\
      f2;					\
    }						\
    printf(del);				\
  } while (0);

/* source and destination matrixes */
shared [BS] double srcmat[MAT_H][MAT_W];
shared [BS] double dstmat[MAT_W][MAT_H];

/* per thread-specific stuff */
shared double time_diffs[NT];
shared int swaps[NT];

/* destination list for bulk writing */
shared void *(dstlist[NT])[UPC_JLS];

void transpose_oopl_co (int i0, int i1, int j0, int j1)
{
  int
    di = i1 - i0,
    dj = j1 - j0;

  if (di >= dj && di > UPC_ILS) {
    int mi = (i0 + i1) / 2;

    transpose_oopl_co (i0, mi, j0, j1);
    transpose_oopl_co (mi, i1, j0, j1);
  }
  else if (dj > UPC_JLS) {
    int mj = (j0 + j1) / 2;

    transpose_oopl_co (i0, i1, j0, mj);
    transpose_oopl_co (i0, i1, mj, j1);
  }
  else {
    int i, j;

#ifdef BULK_TRANSFER_SYNC    
    void *srclist[UPC_JLS];
    double local_block[UPC_JLS][UPC_ILS];

    for (i = i0; i < i1; i++)
      for (j = j0; j < j1; j++)
	local_block[j - j0][i - i0] = srcmat[i][j];
    swaps[T] += di * dj;

    MAT_DO (j1 - j0, i1 - i0,
	    printf ("%5.1f ", local_block[j][i]),
	    printf ("\n"), "===\n");

    for (j = j0; j < j1; j++) {
      srclist[j - j0] = &local_block[j - j0][i - i0];
      dstlist[T][j - j0] = &dstmat[j][i0];
    }

    upc_memput_ilist (j1 - j0, dstlist[T], i1 - i0,
		      j1 - j0, srclist,    i1 - i0);
#endif /* BULK_TRANSFER_SYNC */

#ifdef SPACE_PARTITION_FORALL_TOY
    for (i = i0; i < i1; i++)
      upc_forall (j = j0; j < j1; j++; &dstmat[j][i]) {
	dstmat[j][i] = srcmat[i][j];
	swaps[T]++;
      }
#endif /* SPACE_PARTITION_FORALL_TOY */

#ifdef SPACE_PARTITION
    for (i = i0; i < i1; i++)
      for (j = j0; j < j1; j++)
	dstmat[j][i] = srcmat[i][j];
    swaps[T] += di * dj;
#endif /* SPACE_PARTITION */
  }
}

int main (int argc, char **argv)
{
  timespec_t start, end;
  int
    i,      /* ith matrix dimension */
    j,      /* jth matrix dimension */
    k,      /* kth matrix traversal dimension */
    thread, /* thread number iterator */
    run;    /* number of runs iterator */

  /* fill and print the source matrix */
  if (T == 0) {
    /* fill src matrix */
    MAT_DO (MAT_H, MAT_W,
	    srcmat[i][j] = (double) (i * MAT_W + j),
	    0, "");
#ifdef SHOW_MAT
    /* print src matrix */
    MAT_DO (MAT_H, MAT_W,
	    printf ("%5.1f ", srcmat[i][j]),
	    printf ("\n"), "===\n");
    /* print src matrix affinity */
    MAT_DO (MAT_H, MAT_W,
	    printf ("%5d ", (int) (upc_threadof (&srcmat[i][j]))),
	    printf("\n"), "===\n");
#endif /* SHOW_MAT */
  }

  upc_barrier;

  for (run = 0; run < RUNS; run++) {
    swaps[T] = 0;
    get_time_nsec (&start);

#if defined (SIMPLE_TEST)
    if (T == 0) {
      for (i = 0; i < MAT_H; i++)
	for (j = 0; j < MAT_W; j++)
	  dstmat[j][i] = srcmat[i][j];
      swaps[T] += MAT_H * MAT_W;
    }
#elif defined (SIMPLE_FOR)
    /* won't work with BS=MAT_W/NT */
    for (i = 0; i < MAT_H; i++)
      for (j = T * BS; j < MAT_W; j += NT * BS) {
	dstmat[j][i] = srcmat[i][j];
	swaps[T]++;
      }
#elif defined (SIMPLE_FORALL)
    /* won't work with BS=1 */
    upc_forall (thread = 0; thread < NT; thread++; thread) {
      for (i = 0; i < MAT_H; i++) {
	for (j = T * BS; j < (T * BS) + BS; j++) {
	  dstmat[j][i] = srcmat[i][j];
	  swaps[T]++;
	}
      }
    }
#elif defined (SECTIONED_FORALL)
    /* unbalanced load with BS=MAT_W/NT */
    upc_forall (j = 0; j < MAT_W; j++; (int)((double) j / BS) % MAT_W)
      for (i = 0; i < MAT_H; i++) {
	dstmat[j][i] = srcmat[i][j];
	swaps[T]++;
      }
#elif defined (AFFINITY_FORALL)
    for (i = 0; i < MAT_H; i++)
      upc_forall (j = 0; j < MAT_W; j++; &dstmat[j][i]) {
	dstmat[j][i] = srcmat[i][j];
	swaps[T]++;
      }
#elif defined (LINE_TRANSFER_SYNC)
#elif defined (LINE_TRANSFER_ASYNC)
#elif defined (BULK_TRANSFER_SYNC)
    transpose_oopl_co (0, MAT_H, T * BS, (T * BS) + BS);
#elif defined (BULK_TRANSFER_ASYNC)
#elif defined (PERMUTATION)
#elif defined (SPACE_PARTITION)
    /* will only be useful with BS=MAT_W/NT */
    transpose_oopl_co (0, MAT_H, T * BS, (T * BS) + BS);
#elif defined (SPACE_PARTITION_FORALL_TOY)
    /* this is mostly useless and just a toy */
    transpose_oopl_co (0, MAT_H, 0, MAT_W);
#else /* METHOD */
    printf("Unknown method defined.\n");
#endif /* METHOD */

    get_time_nsec (&end);
    time_diffs[T] = time_diff_nsec (&start, &end);
    upc_barrier;
  }

  upc_barrier;

  if (T == 0) {
#ifdef SHOW_MAT
    /* print dst matrix */
    MAT_DO (MAT_W, MAT_H,
	    printf ("%5.1f ", dstmat[i][j]),
	    printf ("\n"), "===\n");
    /* print dst matrix affinity */
    MAT_DO (MAT_W, MAT_H,
	    printf ("%5d ", (int) (upc_threadof (&dstmat[i][j]))),
	    printf("\n"), "===\n");
#endif /* SHOW_MAT */

#ifdef DO_CHECK
    /* check correctness of dst matrix */
    MAT_DO (MAT_W, MAT_H,
	    if (dstmat[j][i] != (double) (i * MAT_W + j)) {
	      printf("error\n");
	      goto leave;
	    },
	    0, "");
    printf("success\n");
  leave:
#endif /* DO_CHECK */
  }

  if (T == 0) {
    double
      time_diff,         /* time diff of remote thread */
      avg_time_diff,     /* average time diff of all threads */
      largest_time_diff, /* largest time diff of all threads */
      gswaps,            /* gswaps of remote thread */
      avg_gswaps,        /* average gswaps overall */
      overall_gswaps;    /* gswaps/s over largest_time_diff */
    int
      swap,        /* number of swaps of remote thread */
      total_swaps; /* total swaps */

    printf("===\n");

    total_swaps = 0.0;
    avg_time_diff = 0.0;
    largest_time_diff = time_diffs[0];

    for (thread = 0; thread < NT; thread++) {
      time_diff = time_diffs[thread];
      largest_time_diff = MAX(largest_time_diff, time_diff);
      avg_time_diff += time_diff;

      swap = swaps[thread];
      gswaps = ((double) swap / time_diff) / 1e9;
      avg_gswaps += gswaps;
      total_swaps += swap;

      printf ("T %2d: %.15f s, %d swaps, %.15f gswaps/s\n",
  	      thread, time_diff, swap, gswaps);
    }

    printf("\n");

    avg_time_diff /= NT;
    avg_gswaps /= NT;
    overall_gswaps = ((double) total_swaps / largest_time_diff) / 1e9;

    printf("lar_time: %.15f s\n", largest_time_diff);
    printf("avg_time: %.15f s\n", avg_time_diff);

    printf("\n");

    printf("tot_swap: %d swaps\n", total_swaps);
    printf("avg_swap: %.15f gswaps/s\n", avg_gswaps);
    printf("lar_swap: %.15f gswaps/s\n", overall_gswaps);

    printf("===\n");
  }

  return 0;
}
