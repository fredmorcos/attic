#include <stdlib.h>
#include <bsd/bsd.h>

size_t powull_rec (size_t base, size_t exp, size_t acc) {
  if (exp == 0)
    return acc;

  return powull_rec(base, exp - 1, acc * base);
}


void strcatsize_helper (char *str, size_t len, size_t val, size_t div) {
  if (len == 0) {
    *str = '\0';
    return;
  }

  size_t digit = val / div;

  *str = (char) ('0' + digit);
  strcatsize_helper(str + 1, len - 1, val - (digit * div), div / 10);
}

char *strcatsize2 (char *str, size_t *str_len, size_t val) {
  size_t len = 0;
  size_t v = val;
  size_t div = 0;

  char *newstr = NULL;

  do {
    v /= 10;
    len++;
  } while (v);

  newstr = reallocarray(str, *str_len + len + 1, sizeof(char));

  if (!newstr)
    return NULL;

  div = powull_rec(10, len - 1, 1);

  strcatsize_helper(newstr, len, val, div);

  (*str_len) += len;

  return newstr;
}

char *strcatsize (char *str, size_t *str_len, size_t val) {
  size_t len = 0;
  size_t v = val;

  char *newstr = NULL;
  char *p = NULL;
  char *s = NULL;

  do {
    v /= 10;
    len++;
  } while (v);

  newstr = reallocarray(str, *str_len + len + 1, sizeof(char));

  if (!newstr)
    return NULL;

  v = val;
  p = s = newstr + *str_len;

  do {
    *(p++) = (char) ('0' + (v % 10));
    v /= 10;
  } while (v);

  *p = '\0';
  p--;
  (*str_len) += len;

  while (s < p) {
    *s ^= *p;
    *p ^= *s;
    *s ^= *p;
    s++;
    p--;
  }

  return newstr;
}
