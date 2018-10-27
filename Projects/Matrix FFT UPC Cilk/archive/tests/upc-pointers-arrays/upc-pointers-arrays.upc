#include <stdio.h>
#include <upc.h>

#define N  14
#define T  MYTHREAD
#define BS 3

shared      int             x[N];
shared [BS] int             y[N];

shared      int             *a;
shared [BS] int             *b;
            int shared      *c;
            int shared [BS] *d;

int main (int argc, char **argv)
{
  int i;

  if (!T) {
    for (i = 0; i < N; i++) {
      x[i] = i;
      y[i] = i;
    }

    printf("--\n");

    for (i = 0; i < N; i++)
      printf("%3d %3d\n", x[i], y[i]);

    a = &x[0];
    b = (shared [BS] int *) &x[0];
    c = &x[0];
    d = (shared [BS] int *) &x[0];

    for (i = 0; i < N; i++) {
      printf("a = %3d, b = %3d, c = %3d, d = %3d\n", *a, *b, *c, *d);
      a++;
      b++;
      c++;
      d++;
    }

    printf("--\n");

    a = (shared int *) &y[0];
    b = &y[0];
    c = (shared int *) &y[0];
    d = &y[0];

    for (i = 0; i < N; i++) {
      printf("a = %3d, b = %3d, c = %3d, d = %3d\n", *a, *b, *c, *d);
      a++;
      b++;
      c++;
      d++;
    }
  }
  
  return 0;
}
