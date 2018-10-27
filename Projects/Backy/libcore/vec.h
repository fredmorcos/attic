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
#include "attrs.h"

/**
 * A  vector  callback. This  type  is  used for  callbacks  (function
 * pointers). One example is when freeing a vector whose elements need
 * to be freed as well.
 */
typedef void (vec_cb)(void *const);

/**
 * A generic vector structure.
 *
 * *ptr is the pointer to the actual C array, len is the used (actual)
 * length  of the  vector (in  number of  elements), cap  is the  full
 * capacity (capacity  = length +  free) of  the vector (in  number of
 * elements), chunk is  a heuristic increment (in  number of elements)
 * when the vector is full (length = capacity), esize is the size of a
 * single element of the vector (in  bytes) and free_cb is an optional
 * (possibly  NULL)  callback for  freeing  vector  elements when  the
 * vector itself is destroyed.
 */
struct vec {
  void   *ptr;
  size_t  len;
  size_t  cap;
  size_t  chunk;
  size_t  esize;
  vec_cb *free_cb;
};

/**
 * vec_init(*vec, chunk, element_size, free_callback).
 *
 * This   initializes  an   already  allocated   vector  with   chunk,
 * element_size and free_callback.  free_callback  is optional and can
 * be  NULL  to completely  disable  any  callbacks when  freeing  the
 * vector.
 */
void vec_init(struct vec *const self,
              const size_t chunk,
              const size_t esize,
              vec_cb *const free_cb)
  attr_nonnull((1));

/**
 * vec_free(*vec).
 *
 * This frees a vector and executes its free callback if set.
 */
void vec_free(struct vec *const self)
  attr_nonnull_all;

/**
 * pointer vec_addn(*vec, n).
 *
 * This adds  space to the  vector (by  reallocation if needed)  for n
 * elements (n *  esize bytes). In case of error,  it returns NULL. In
 * case of success, it returns a pointer to an address within an array
 * where the n new elements start.
 */
void *vec_addn(struct vec *const self, const size_t n)
  attr_nonnull_all
  attr_malloc
  attr_warn_unused_result;

/**
 * bool vec_extn(*vec, n).
 *
 * This works like  vec_addn() but does not return  a pointer. Instead
 * it  returns true  on success  and false  on error.  Can be  used to
 * preallocate a vector  and avoid the compiler  warning stemming from
 * the __attribute__((warn_unused_result)) on vec_addn().
 */
bool vec_extn(struct vec *const self, const size_t n)
  attr_nonnull_all;

/**
 * vec_revn(*vec, n).
 *
 * This simply  reverts a previous vec_addn()  or vec_extn() operation
 * by n  elements (n * esize  bytes). No deallocation or  shrinking is
 * made.
 */
void vec_revn(struct vec *const self, const size_t n)
  attr_nonnull_all;
