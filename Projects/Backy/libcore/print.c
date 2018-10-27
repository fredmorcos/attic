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
#include <err.h>
#include "str.h"
#include "print.h"

char *humansize(long double size) {
  assert(size > 0);

  static const char units[] = "BKMGTPE";
  static const size_t units_len = sizeof(units);

  char *res = NULL;
  size_t i = 0;

  while (size >= 1024 && i < units_len) {
    size /= 1024;
    i++;
  }

  return xasprintf(NULL, &res, "%.2Lf%c", size, units[i])
    < 0 ? NULL : res;
}

void print_bufstats(const size_t nent,
                    const size_t size,
                    const struct time_info *const ti) {
  assert(ti != NULL);
  assert(ti->cpu >= 0);
  assert(ti->rt >= 0);
  assert(ti->wc >= 0);

  char *bufsize = NULL;

  if (nent == 0) {
    assert(size == 1);
    warnx("OK: Loaded %zu entries", nent);
  } else if ((bufsize = humansize(size))) {
    warnx("OK: Loaded %zu entries into a %s buffer "
          "in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
          nent, bufsize, ti->cpu, ti->rt, ti->wc);
    free(bufsize);
  } else {
    warnx("OK: Loaded %zu entries into a %zu bytes buffer "
          "in %.2Lfs cpu, %.2Lfs rt, %.2Lfs wc",
          nent, size, ti->cpu, ti->rt, ti->wc);
  }
}
