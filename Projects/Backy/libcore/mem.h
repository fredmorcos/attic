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

/**
 * void *xrealloc(void **pointer, size_t nmembers, size_t member_size)
 *
 * Will  reallocate *pointer  to a  memory  chunk of  size nmembers  *
 * member_size.  In  case of  overflow, xrealloc()  will set  errno to
 * EOVERFLOW and return NULL. In the case of memory allocation errors,
 * errno will  be set  according to realloc(),  *pointer will  be left
 * unchanged and xrealloc()  will return NULL.  In  case of successful
 * allocation, *pointer will be set to the new value (which may or may
 * not be equal to the old one) and xrealloc() will return *pointer.
 *
 * Note: Do not  check for success by comparing old  and new values of
 * *pointer. Use  the return value  (NULL on error, other  on success)
 * for that.
 */
void *xrealloc(void **p, const size_t n, const size_t size)
  attr_alloc_size_prod(2, 3)
  attr_nonnull_all
  attr_malloc
  attr_warn_unused_result;
