#pragma once

#include <stdbool.h>
#include <time.h>
#include <sys/time.h>

/**
 * A   structure  that   contains  time   and  time   delta/difference
 * information. *_cpu  are for CPU  time, *_rt  are for real  time and
 * *_wc are for wall-clock time.
 */
struct time_info {
  clock_t begin_cpu;
  clock_t end_cpu;

  struct timespec begin_rt;
  struct timespec end_rt;

  struct timeval begin_wc;
  struct timeval end_wc;

  /* the deltas */
  long double cpu;
  long double wc;
  long double rt;
};

#define TI_FMT "%.2Lfs cpu, %.2Lfs rt, %.2Lfs wc"
#define TI_FMT_PARAMS(S) S.cpu, S.rt, S.wc

void ti_now(struct time_info *const ti, const bool begin)
  __attribute__((nonnull));

void ti_diff(struct time_info *const ti)
  __attribute__((nonnull));
