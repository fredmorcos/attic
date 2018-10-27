/*
 * Copyright (c) 2015-2016, Fred Morcos <fred.morcos@gmail.com>
 *
 * Permission to  use, copy,  modify, and/or distribute  this software
 * for any  purpose with  or without fee  is hereby  granted, provided
 * that the above  copyright notice and this  permission notice appear
 * in all copies.
 *
 * THE  SOFTWARE IS  PROVIDED "AS  IS"  AND THE  AUTHOR DISCLAIMS  ALL
 * WARRANTIES  WITH  REGARD TO  THIS  SOFTWARE  INCLUDING ALL  IMPLIED
 * WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO EVENT  SHALL THE
 * AUTHOR   BE  LIABLE   FOR   ANY  SPECIAL,   DIRECT,  INDIRECT,   OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF  USE,  DATA  OR  PROFITS,  WHETHER IN  AN  ACTION  OF  CONTRACT,
 * NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,   ARISING  OUT  OF  OR  IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include "time.h"

void ti_now(struct time_info *const ti, const bool begin) {
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

void ti_diff(struct time_info *const ti) {
  assert(ti != NULL);

  static const unsigned int million = 1000000;
  static const unsigned int billion = 1000000000L;

  time_t sec, fsec;

  ti_now(ti, false);

  /* cpu time */
  ti->cpu = (ti->end_cpu - ti->begin_cpu) / (long double) CLOCKS_PER_SEC;
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
