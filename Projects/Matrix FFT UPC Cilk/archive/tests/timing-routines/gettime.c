#include "gettime.h"

#include <time.h>

static struct timespec begin_t, end_t;

void starttime()
{
  clock_gettime(CLOCK_REALTIME, &begin_t);
}

double endtime_old()
{
  long sec, nsec;

#ifndef TEST
  clock_gettime(CLOCK_REALTIME, &end_t);
#endif /* TEST */

  if (end_t.tv_sec == begin_t.tv_sec) {
    nsec = end_t.tv_nsec - begin_t.tv_nsec;
    sec  = end_t.tv_sec - begin_t.tv_sec;
  } else {
    nsec = (end_t.tv_nsec + 100) - begin_t.tv_nsec;
    sec  = end_t.tv_sec - begin_t.tv_sec - 1;
  }

  return (sec + 1.e-2 * nsec);
}

double endtime()
{
  long sec, nsec;

#ifndef TEST
  clock_gettime(CLOCK_REALTIME, &end_t);
#endif /* TEST */

  if (end_t.tv_nsec < begin_t.tv_nsec) {
    nsec = (end_t.tv_nsec + 100) - begin_t.tv_nsec;
    sec  = end_t.tv_sec - begin_t.tv_sec - 1;
  } else {
    nsec = end_t.tv_nsec - begin_t.tv_nsec;
    sec  = end_t.tv_sec - begin_t.tv_sec;
  }

  return (sec + 1.e-2 * nsec);
}

#ifdef TEST
#include <stdio.h>

int main (int argc, char **argv)
{
  int bs, bn, es, en;
  double endtime_res, endtime_old_res, sub_res;

  for (bs = 0; bs < 100; bs++)
    for (bn = 0; bn < 100; bn++)
      for (es = bs; es < 100; es++)
	for (en = 0 ? bs < es : bn; en < 100; en++) {
	  begin_t.tv_sec = bs;
	  begin_t.tv_nsec = bn;

	  end_t.tv_sec = es;
	  end_t.tv_nsec = en;

	  endtime_res = endtime();
	  endtime_old_res = endtime_old();
	  sub_res = (double) ((((double) es) + (((double) en) * 1.e-2)) - \
			      (((double) bs) + (((double) bn) * 1.e-2)));

	  if (endtime_res != sub_res ||
	      fabs(endtime_res - endtime_old_res) > 1.e-12) {
	    printf("Error.\n");
	    printf("%.20f %.20f %.20f\n", endtime_res, endtime_old_res, sub_res);
	    printf("  endtime - sub = %e, endtime - endtime_old = %e\n",
		   endtime_res - sub_res, endtime_res - endtime_old_res);
	    printf("bs = %d, bn = %d, es = %d, en = %d\n", bs, bn, es, en);
	    return 1;
	  } /* else { */
	  /*   printf("%.9f %.9f %.9f\n", endtime_res, endtime_old_res, sub_res); */
	  /* } */
	}

  return 0;
}
#endif /* TEST */
