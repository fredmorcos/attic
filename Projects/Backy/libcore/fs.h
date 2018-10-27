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
#include <dirent.h>
#include "attrs.h"

char *xrealpath(const char *const p)
  attr_nonnull_all
  attr_malloc
  attr_warn_unused_result;

bool xchdir(DIR *const d, const char **const err_msg)
  attr_nonnull_all;

void xclosedir(DIR **d)
  attr_nonnull_all;

bool xfstat(const char *const path,
            time_t *const mtime,
            long long *const size)
  attr_nonnull_all;

bool xlstatlen(const char *const path,
               size_t *const target_len,
               const char **const err_msg,
               char **const err_path)
  attr_nonnull_all;

bool xlstat(const char *const path,
            char *const t,
            const size_t tlen,
            const char **const err_msg,
            char **const err_path)
  attr_nonnull_all;
