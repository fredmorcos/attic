#include <assert.h>
#include <errno.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "obj.h"
#include "str.h"

static const char NUL = '\0';

static void str_destroy(struct str *const str);

int str_init(struct str *const str,
             const bool autofree,
             const char *const cstr) {
  obj_init(&str->parent, autofree, (obj_dtor *) str_destroy);
  str->len = 0;
  str->cstr = NULL;
  return str_set(str, cstr);
}

static void str_destroy(struct str *const str) {
  assert(str);

  if (str->cstr) {
    free(str->cstr);
    str->cstr = NULL;
  }
}

int str_set(struct str *const str,
            const char *const cstr) {
  assert(str);
  str_destroy(str);

  if (cstr) {
    size_t cstr_len = strlen(cstr);
    str->cstr = malloc(sizeof(char) * (cstr_len + 1));

    if (str->cstr)
      return -errno;

    (void) strncpy(str->cstr, cstr, cstr_len);
    str->cstr[cstr_len] = NUL;
  }

  return 0;
}
