#include "string.h"

#include <assert.h>
#include <string.h>
#include <bsd/bsd.h>

void string_init(String *const s, const size_t chunk_len) {
  (void) memset(s, 0, sizeof(String));
  s->clen = chunk_len;
}

void string_free(String *const s) {
  if (s->alen == 0) {
    assert(s->len == 0);
    assert(s->ptr == NULL);
  } else {
    assert(s->ptr != NULL);
    free(s->ptr);
  }
}

void string_revert(String *const s, const size_t n) {
  assert(s->len >= n);
  s->len -= n;
}

char *string_extend(String *const s, const size_t n) {
  assert(s->len <= s->alen);

  if (n != 0 && s->len + n >= s->alen) {
    char   *new_ptr  = NULL;
    size_t  new_alen = 0;

    new_alen = n > s->alen ? s->alen + n : s->alen * 2;
    new_alen = new_alen > s->clen ? new_alen : s->clen;

    new_ptr = reallocarray(s->ptr, new_alen + 1, sizeof(char));

    if (new_ptr == NULL)
      return NULL;

    assert(new_ptr != NULL);

    s->ptr  = new_ptr;
    s->alen = new_alen;

    assert(s->ptr != NULL);
  }

  {
    char *ptr = NULL;

    assert(s->ptr != NULL);

    ptr = s->ptr + (s->len * sizeof(char));
    *(ptr + sizeof(char)) = 0;
    s->len += n;

    return ptr;
  }
}

char *string_extend_zero(String *const s, const size_t n) {
  char *ptr = NULL;

  ptr = string_extend(s, n);

  if (ptr == NULL)
    return NULL;

  (void) memset(ptr, 0, n * sizeof(char));

  return ptr;
}
