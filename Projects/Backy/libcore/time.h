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

#pragma once

#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include "attrs.h"

/**
 * A   structure    that   contains    time   and    time   difference
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

  long double cpu;
  long double wc;
  long double rt;
};

/**
 * ti_now(*time_info, bool begin).
 *
 * This stores the current (CPU, RT,  WC) times into either begin_* or
 * end_* markers.  If begin is  true, time information is  stored into
 * the begin_*  markers, and  if begin is  false, time  information is
 * stored into end_* markers.
 */
void ti_now(struct time_info *const ti, const bool begin)
  attr_nonnull_all;

/**
 * ti_diff(*time_info).
 *
 * This stores the differences in (CPU, RT, WC) times from begin_* and
 * end_* markers into the ldoubles  cpu, wc, and rt respectively. Note
 * that there  is no need to  call ti_now() with begin  = false before
 * calling ti_diff(),  it already  does that at  the beginning  of the
 * function.
 */
void ti_diff(struct time_info *const ti)
  attr_nonnull_all;
