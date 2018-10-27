#include <stdio.h>
#include <math.h>

#include "gettime.h"
#include "test-common.h"

int time_diff_usec_test()
{
  int bs, bu, es, eu;
  double endtime_res, sub_res;
  timeval_t begin, end;

#ifdef __GNUC__
  double err = 1.e-8;
#else
  double err = 1.e-12;
#endif

  for (bs = 0; bs < 100; bs++)
    for (bu = 0; bu < 100; bu++)
      for (es = bs; es < 100; es++)
	for (eu = 0 ? bs < es : bu; eu < 100; eu++) {
	  begin.tv_sec = bs;
	  begin.tv_usec = bu;

	  end.tv_sec = es;
	  end.tv_usec = eu;

	  endtime_res = time_diff_usec(&begin, &end);
	  sub_res = (double) ((((double) es) + (((double) eu) * 1.e-6)) - \
			      (((double) bs) + (((double) bu) * 1.e-6)));

	  if (fabs(endtime_res - sub_res) > err) {
	    printf("Error.\n");
	    printf("%.40f %.40f\n", endtime_res, sub_res);
	    printf("endtime_res - sub_res = %e\n", endtime_res - sub_res);
	    printf("bs = %d, bu = %d, es = %d, eu = %d\n", bs, bu, es, eu);

	    return 1;
	  }
	}

  return 0;
}

int time_diff_nsec_test()
{
  int bs, bn, es, en;
  double endtime_res, sub_res;
  timespec_t begin, end;

#ifdef __GNUC__
  double err = 1.e-11;
#else
  double err = 1.e-18;
#endif

  for (bs = 0; bs < 100; bs++)
    for (bn = 0; bn < 100; bn++)
      for (es = bs; es < 100; es++)
	for (en = 0 ? bs < es : bn; en < 100; en++) {
	  begin.tv_sec = bs;
	  begin.tv_nsec = bn;

	  end.tv_sec = es;
	  end.tv_nsec = en;

	  endtime_res = time_diff_nsec(&begin, &end);
	  sub_res = (double) ((((double) es) + (((double) en) * 1.e-9)) - \
			      (((double) bs) + (((double) bn) * 1.e-9)));

	  if (fabs(endtime_res - sub_res) > err) {
	    printf("Error.\n");
	    printf("%.40f %.40f\n", endtime_res, sub_res);
	    printf("endtime_res - sub_res = %e\n", endtime_res - sub_res);
	    printf("bs = %d, bn = %d, es = %d, en = %d\n", bs, bn, es, en);

	    return 1;
	  }
	}

  return 0;
}

int main (int argc, char **argv)
{
  printf("Running gettime_test...\n");
  RUN_TEST_FUNC(time_diff_usec_test);
  RUN_TEST_FUNC(time_diff_nsec_test);
  return 0;
}
