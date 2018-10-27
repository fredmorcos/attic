#pragma once

#include <stdlib.h>

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
  __attribute__((malloc, warn_unused_result));
