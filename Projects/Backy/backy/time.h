#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <time.h>
#include <sys/time.h>

typedef struct timespec TimeSpec;
typedef struct timeval  TimeVal;

/**
 * A   structure   that    contains   time   and   time
 * delta/difference  information.  *_cpu  are  for  CPU
 * time,  *_rt  are for  real  time  and *_wc  are  for
 * wall-clock time.
 */
struct time_info {
  clock_t begin_cpu;
  clock_t end_cpu;

  TimeSpec begin_rt;
  TimeSpec end_rt;

  TimeVal begin_wc;
  TimeVal end_wc;

  /* the deltas */
  long double cpu;
  long double wc;
  long double rt;
};

typedef struct time_info TimeInfo;

#define TI_FMT \
  "%.2Lfs cpu, %.2Lfs rt, %.2Lfs wc"

#define TI_FMT_PARAMS(S) \
  S.cpu, S.rt, S.wc

void ti_now(TimeInfo *const ti, const bool begin)
  attr((nonnull));

void ti_diff(TimeInfo *const ti)
  attr((nonnull));
