#include <xmmintrin.h>
#include <stdio.h>

typedef union
{
  __m128 m; 
  float f[4];
} vector;

float
skalarprodukt(float x[], float y[])
{
  return x[0]*y[0] + x[1]*y[1] + x[2]*y[2] + x[3]*y[3];
}

float
skalarproduktAssembler(float x[], float y[])
{
  vector tmp;
  asm("movl   %1,       %%esi;"
      "movl   %2,       %%edi;"
      "movaps (%%esi),  %%xmm0;"
      "mulps  (%%edi),  %%xmm0;"
      "movaps %%xmm0,   %0;"
      :"=g"(tmp)        /* Ausgabe */
      :"r"(x),"r"(y)    /* Eingabe */
      ); 
  
  return tmp.f[0] + tmp.f[1] + tmp.f[2] + tmp.f[3];
}

float
skalarproduktIntrinsics(float x[], float y[])
{
  __m128 *vecX = (__m128 *)x;
  __m128 *vecY = (__m128 *)y;
  vector tmp;
  tmp.m = _mm_mul_ps(vecX[0],vecY[0]);
  return tmp.f[0] + tmp.f[1] + tmp.f[2] + tmp.f[3];
}

int
main()
{  
  float x[4] = {1.0, 2.0, 3.0, 4.0};
  float y[4] = {5.0, 6.0, 7.0, 8.0};
  printf("3: %f\n", skalarproduktIntrinsics(x, y));  
  printf("2: %f\n", skalarproduktAssembler(x, y));
  printf("1: %f\n", skalarprodukt(x, y));
 
  
  return 0;
}
