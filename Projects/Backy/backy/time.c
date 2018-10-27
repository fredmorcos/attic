#include <assert.h>
#include "time.h"

void ti_now(TimeInfo *const ti, const bool begin) {
  assert(ti != NULL);

  if (begin) {
    ti->begin_cpu = clock();
    clock_gettime(CLOCK_REALTIME, &ti->begin_rt);
    gettimeofday(&ti->begin_wc, NULL);
  } else {
    ti->end_cpu = clock();
    clock_gettime(CLOCK_REALTIME, &ti->end_rt);
    gettimeofday(&ti->end_wc, NULL);
  }
}

void ti_diff(TimeInfo *const ti) {
  assert(ti != NULL);

  static const unsigned int million = 1000000;
  static const unsigned int billion = 1000000000L;

  time_t sec, fsec;

  ti_now(ti, false);

  /* cpu time */
  ti->cpu =
    (ti->end_cpu - ti->begin_cpu) /
    (long double) CLOCKS_PER_SEC;

  assert(ti->cpu >= 0);

  /* real-time */
  if (ti->begin_rt.tv_nsec > ti->end_rt.tv_nsec) {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec - 1;
    fsec = ti->end_rt.tv_nsec + billion - ti->begin_rt.tv_nsec;
  } else {
    sec  = ti->end_rt.tv_sec - ti->begin_rt.tv_sec;
    fsec = ti->end_rt.tv_nsec - ti->begin_rt.tv_nsec;
  }

  ti->rt = sec + (long double) 1.e-9 * fsec;
  assert(ti->rt >= 0);

  /* wall-clock time */
  if (ti->begin_wc.tv_usec > ti->end_wc.tv_usec) {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec - 1;
    fsec = ti->end_wc.tv_usec + million - ti->begin_wc.tv_usec;
  } else {
    sec = ti->end_wc.tv_sec - ti->begin_wc.tv_sec;
    fsec = ti->end_wc.tv_usec - ti->begin_wc.tv_usec;
  }

  ti->wc = sec + (long double) 1.e-6 * fsec;
  assert(ti->wc >= 0);
}
