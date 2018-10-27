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

#include <stdlib.h>
#include "attrs.h"

int xasprintf(size_t *const l, char **const s, const char *const f, ...)
  attr_nonnull((2, 3))
  attr_format(printf, 3, 4);

size_t xstrlcpy(char *dst, const char *src, const size_t dsize)
  attr_nonnull_all;
