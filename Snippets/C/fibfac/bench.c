#include "bench.h"

#include <sys/time.h>

static struct timeval begin_tp, end_tp;

void starttime()
{
   gettimeofday(&begin_tp, (struct timezone *) 0);
}

double endtime()
{
   int sec, usec;

   gettimeofday(&end_tp, (struct timezone *) 0);

   if (end_tp.tv_sec == begin_tp.tv_sec) {
      usec = end_tp.tv_usec - begin_tp.tv_usec;
      sec  = end_tp.tv_sec - begin_tp.tv_sec;
   }
   else {
      usec = end_tp.tv_usec + 1000000 - begin_tp.tv_usec;
      sec  = end_tp.tv_sec - begin_tp.tv_sec - 1;
   }

   return (sec + 1.e-6 * usec);
}
