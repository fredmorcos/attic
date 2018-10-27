#include "utility.h"
#include "common.h"

void print_array(double** m, int dim) {
  int row, col;

  for (row = 0; row < dim; row++) {
    printf("\n|");
    for (col = 0; col < dim; col++)
      printf("%12.2f", m[row][col]);
    printf("|");
  }
  printf("\n");
}

void free_array_slow(double** m, int dim) {
  int i;
  for (i = 0; i < dim; i++)
    free(m[i]);

  free(m);
}

void free_array(double** m) {
  free(m[0]);
  free(m);
}

double getMaxFlops() {
#define MHZ "cat /proc/cpuinfo |grep 'cpu MHz'|awk '{print $4}'|tail -n1"
  FILE *ptr, *popen();
  char result[128];
  double mhz;

  if ((ptr = popen(MHZ, "r")) == NULL)
    fprintf(stderr, "could not open pipe\n");
  while (1) {
    if (fgets(result, 128, ptr) == NULL) break;
  }
  fclose(ptr);
  mhz = atof(result);

  return mhz*2*pow(10,6)/pow(10,9);
}


double calcMean(double *arr, int size) {
  switch (size) {
  case 0: return 0;
  case 1: return arr[0];
  case 2: return (arr[0]+arr[1])/2;
  default: {
    int i;
    double max = arr[0];												
    double min = arr[0];
    double meantime = 0;
    for (i = 0; i < size; i++) {
      if (arr[i] > max)	max = arr[i];
      if (arr[i] < min)	min = arr[i];
    }
    for (i = 0; i < size; i++) 
      if (arr[i] != max && arr[i] != min) meantime += arr[i];
	
    return meantime/(size-2);
  }
  }
  return 0;
}

void alloc_array_slow(double*** m, int dim) {
  int row;
  *m = (double**) calloc(dim, sizeof(double*));

  if (*m == NULL)
    error_exit("not enough memory");

  for (row = 0; row < dim; row++) {
    (*m)[row] = (double*) calloc(dim, sizeof(double));
    if ((*m)[row] == NULL) 
      error_exit("not enough memory");
  }
}

void alloc_array(double*** m, int dim) {
  int row;
  *m = (double**) calloc(dim, sizeof(double*));
  **m = (double*) calloc(dim * dim, sizeof(double));
  if (*m == NULL || **m == NULL) {
    puts("\nNot enough memory for array allocation.\n");
    exit(0);
  }

  for (row = 0; row < dim; row++) 
    (*m)[row] = (**m) + row*dim;
}

void init_array(double** m, int dim, int value) {
  int row, col;

  for (row = 0; row < dim; row++) {
    for (col = 0; col < dim; col++) {
      m[row][col] = (value == 0) ? 0 : rand()%10000;
    }
  }
}
