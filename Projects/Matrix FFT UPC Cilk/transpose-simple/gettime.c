#include "gettime.h"

void get_time_nsec(timespec_t *tp)
{
  clock_gettime(CLOCK_REALTIME, tp);
}

double time_diff_nsec(timespec_t *begin, timespec_t *end)
{
  time_t sec;
  long nsec;

  if (begin->tv_nsec > end->tv_nsec) {
    sec  = end->tv_sec - begin->tv_sec - 1;
    nsec = end->tv_nsec + 1000000000L - begin->tv_nsec;
  } else {
    sec  = end->tv_sec - begin->tv_sec;
    nsec = end->tv_nsec - begin->tv_nsec;
  }

  return (sec + 1.e-9 * nsec);
}
