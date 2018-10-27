/*
 * Copyright (c) 2016, Fred Morcos <fred.morcos@gmail.com>
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

/**
 * attr_alloc_size(n), attr_alloc_size_prod(n, m)
 *
 * These are function attributes.
 *
 * attr_alloc_size()  marks  the  function  to  be  returning  a  heap
 * allocation of a size that  is requested in the function's parameter
 * number n.
 *
 * attr_alloc_size_prod() marks  the function  to be returning  a heap
 * allocation of a size that is calculated from the product of the nth
 * and mth function parameters.
 *
 * Note that parameter indexing starts at 1.
 *
 * Note that  clang does not  support the alloc_size attribute  and so
 * these macros expand to nothing.
 *
 * Examples:
 *  void *malloc(..., x) attr_alloc_size(2);
 *  void *realloc(..., x, y) attr_alloc_size_prod(2, 3);
 */
#if defined(__clang__)
#define attr_alloc_size(x)
#define attr_alloc_size_prod(x,y)
#else
#define attr_alloc_size(x)        __attribute__((alloc_size(x)))
#define attr_alloc_size_prod(x,y) __attribute__((alloc_size(x,y)))
#endif

/**
 * attr_malloc
 *
 * This is a function attribute.
 *
 * Marks the function  to be a malloc()-like function,  which means it
 * should return a pointer to a heap allocated object.
 *
 * Examples:
 *  struct obj *obj_new(...) attr_malloc;
 */
#define attr_malloc __attribute__((malloc))

/**
 * attr_warn_unused_result
 *
 * This is a function attribute.
 *
 * Issue a warning when the function's return value is unused.
 */
#define attr_warn_unused_result __attribute__((warn_unused_result))

/**
 * attr_pure
 *
 * This is a function attribute.
 *
 * Marks the  function as pure,  meaning it has no  side-effects other
 * than its return value. Allows the compiler to optimize the function
 * just as it  would optimize arithmetic operations (ie,  in common or
 * repeated sub-expressions).
 */
#define attr_pure __attribute__((pure))

/**
 * attr_const
 *
 * This is a function attribute.
 *
 * Marks  the function  as const,  which has  a stronger  meaning than
 * attr_pure. A const function cannot access memory outside of its own
 * stack frame.
 */
#define attr_const __attribute__((const))

/**
 * attr_format(type, 2, 3)
 *
 * This is a function attribute.
 *
 * Mark   the  function   as  a   printf(),  scanf(),   strftime()  or
 * strfmon()-like  function.   The  compiler  is then  able  to  issue
 * warnings about  type mismatches between  the format string  and the
 * arguments list.
 *
 * Note that parameter indexing starts at 1.
 *
 * Examples:
 *  void obj_print(..., format_string, ...) attr_format(printf, 2, 3);
 */
#define attr_format(t,x,y) __attribute__((format(t,x,y)))

/**
 * attr_nonnull_all
 *
 * This is a function attribute.
 *
 * Mark the function as not accepting  any NULL values for its pointer
 * arguments.
 *
 * Note that this is a simplistic compile-time check. Assertions would
 * still be needed for run-time checks.
 */
#define attr_nonnull_all __attribute__((nonnull))

/**
 * attr_nonnull(n1, n2, ...)
 *
 * This is a function attribute.
 *
 * Marks the function as not accepting any NULL values for its pointer
 * arguments at indexes n1, n2, etc...
 *
 * Note that parameter indexing starts at 1.
 *
 * Note that this is a simplistic compile-time check. Assertions would
 * still be needed for run-time checks.
 */
#define attr_nonnull(x) __attribute__((nonnull x))

/**
 * attr_returns_nonnull
 *
 * This is a function attribute.
 *
 * Marks the function as not returning NULL.
 *
 * Note that this is a simplistic compile-time check. Assertions would
 * still be needed for run-time checks.
 */
#define attr_returns_nonnull __attribute__((returns_nonnull))

/**
 * attr_unused
 *
 * This is a function argument attribute.
 *
 * Disable warnings about unused function arguments.
 *
 * Examples:
 *  void func(attr_unused int x, int y);
 */
#define attr_unused __attribute__((unused))

/**
 * attr_cleanup(cleanup_function)
 *
 * This is a variable attribute.
 *
 * Declare a  cleanup function for  a variable. The function  shall be
 * executed  when  the variable  goes  out  of scope  (either  through
 * exiting the current  block scope, or through  return). The function
 * shall have a  return type of void  and take an argument  of type (T
 * *const)   if  declared   for   a  variable   of   type  (T):   void
 * cleanup_function (T *const);
 *
 * Note that this does not work when using exit().
 */
#define attr_cleanup(x) __attribute__((cleanup(x)))

/**
 * attr_designated_init
 *
 * This is a struct attribute.
 *
 * Forces the  use of designated  initializers on structs.   Useful to
 * issue a warning  otherwise, in cases where a  struct's structure is
 * planned to change.
 *
 * Note that clang does not  support the designated_init attribute and
 * so this macro expands to nothing.
 *
 * Examples:
 *  struct ... { ... } attr_designated_init;
 */
#if defined(__clang__)
#define attr_designated_init
#else
#define attr_designated_init __attribute__((designated_init))
#endif
