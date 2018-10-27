#include <stdio.h>
#include <upc.h>

int main (int argc, char **argv)
{
  /* WILL NOT WORK

     UPC doesn't support shared auto variables.

     if a thread goes out of scope (returns from 
     the main function), all local variables 
     (including shared ones) will be "free'd".

     shared variables cannot have automatic storage
     duration.
  */
  shared int step = 10;
  int fah, cel;

  cel = step * MYTHREAD;
  fah = cel * (9.0 / 5.0) + 32;

  printf("%d \t %d \n", fah, cel);
  return 0;
}
