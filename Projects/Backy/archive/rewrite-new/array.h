#pragma once

#include <stdbool.h>
#include <bsd/bsd.h>

struct array {
  size_t len;                           /* used len */
  size_t alen;                          /* alloc'ed len */
  size_t esize;                         /* element size */
  void (*free_cb)(void *);              /* free callback */

  uint8_t ptr[];                        /* actual array */
};

typedef void (*const array_cb)(void *);

void array_init (struct array *const a, const size_t esize, array_cb cb);
void array_revn (struct array *const a, const size_t n);
bool array_addn (struct array *const a, const size_t n,
                 const bool zero, void **const res);
void array_free (struct array *const a);
