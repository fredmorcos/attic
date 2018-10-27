#ifndef __UTILS_H__
#define __UTILS_H__

/* sizeof shortcuts */
#define SO_F sizeof(float)
#define SO_C sizeof(char)
#define SO_D sizeof(double)

/* initialization */
#define init(x, n, m)				\
  do {						\
    if (T == 0) {				\
      init_matrix(x, n, m);			\
      print_matrix(x, n, m);			\
      /* print_affinity(x, n, m); */		\
    }						\
  } while (0);

/* finalization, ie, printing and checks */
#define finish(x, n, m)				\
  do {						\
    if (T == 0) {				\
      print_matrix(x, n, m);			\
      check_matrix(x, n, m);			\
      print_info;				\
    }						\
  } while (0);

/* print all timing info from all threads */
/* #define print_info				\ */
/*   do {						\ */
/*     int t;					\ */
/*     for (t = 0; t < NT; t++)			\ */
/*       printf("T %2d: %15.10f s, %d swaps\n",	\ */
/* 	     t, time_diffs[t], swaps[t]);	\ */
/*   } while (0); */

/* fill matrix with values row-major */
/* #define init_matrix(x, n, m)			\ */
/*   do {						\ */
/*     printf("initializing... ");			\ */
/*     int i, j;					\ */
/*     for (i = 0; i < n; i++)			\ */
/*       for (j = 0; j < m; j++)			\ */
/* 	x[i][j] = (float)(i * m + j);		\ */
/*     printf("done\n");				\ */
/*   } while (0); */

#ifdef DO_CHECK

/* check the matrix values column-major
   as if transposed, used to check the
   correctness of the transposition */
/* #define check_matrix(x, n, m)			\ */
/*   do {						\ */
/*     printf("checking... ");			\ */
/*     int i, j;					\ */
/*     for (i = 0; i < n; i++)			\ */
/*       for (j = 0; j < m; j++)			\ */
/* 	if (x[j][i] != (float)(i * m + j)) {	\ */
/* 	  printf("error\n");			\ */
/* 	  goto bye;				\ */
/* 	}					\ */
/*     printf("success\n");			\ */
/*   bye:						\ */
/*   } while (0); */

#endif /* DO_CHECK */

#ifdef SHOW_MATRIX

/* print matrix row-major, for debugging */
/* #define print_matrix(x, n, m)			\ */
/*   do {						\ */
/*     int i, j;					\ */
/*     printf("==\n");				\ */
/*     for (i = 0; i < n; i++) {			\ */
/*       for (j = 0; j < m; j++)			\ */
/* 	printf("%5.1f ", x[i][j]);		\ */
/*       printf("\n");				\ */
/*     }						\ */
/*     printf("==\n");				\ */
/*   } while (0); */

/* print thread-affinity of matrix, for debugging */
#define print_affinity(x, n, m)				\
  do {							\
    int i, j;						\
    printf("==\n");					\
    for (i = 0; i < n; i++) {				\
      for (j = 0; j < m; j++)				\
	printf("%5d ", (int) upc_threadof(&x[i][j]));	\
      printf("\n");					\
    }							\
    printf("==\n");					\
  } while (0);
#else
#define print_matrix(x, n, m)
#define print_affinity(x, n, m)
#endif /* SHOW_MATRIX */

#endif /* __UTILS_H__ */
