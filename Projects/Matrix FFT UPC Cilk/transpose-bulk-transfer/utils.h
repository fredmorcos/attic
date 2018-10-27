#ifndef __UTILS_H__
#define __UTILS_H__

#define init_matrix(x, n, m)			\
  do {						\
    int i, j;					\
    for (i = 0; i < n; i++)			\
      for (j = 0; j < m; j++)			\
	x[i][j] = (float)(i * m + j);		\
  } while (0);

#ifdef SHOW_MATRIX

#define print_matrix(x, n, m)			\
  do {						\
    int i, j;					\
    printf("==\n");				\
    for (i = 0; i < n; i++) {			\
      for (j = 0; j < m; j++)			\
	printf("%5.1f ", x[i][j]);		\
      printf("\n");				\
    }						\
    printf("==\n");				\
  } while (0);

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
