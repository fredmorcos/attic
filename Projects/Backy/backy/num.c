#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "num.h"

unsigned int ndigits(ull val) {
  unsigned int len = 0;

  do {
    val /= 10;
    len++;
  } while (val);

  return (len);
}

void ulltoa(char *str, ull val, const uint len) {
  assert(str != NULL);
  assert(len > 0);

  double res = pow(10, len - 1);

  assert(res > 0);
  assert(res <= LLONG_MAX);

  ull div = (ull) llround(res);

  for (ull digit = 0; div > 0; div /= 10) {
    digit = val / div;
    *(str++) = (char) ('0' + digit);
    val -= digit * div;
  }
}

/* Modifed from OpenBSD to be rather minimal */
bool strtonum(const char *const numstr,
              long long minval,
              long long maxval,
              long long *const res) {
  assert(numstr != NULL);
  assert(res != NULL);

  long long ll = 0;
  char *ep;

  if (minval > maxval) {
    errno = EINVAL;
    return (false);
  }

  errno = 0;
  ll = strtoll(numstr, &ep, 10);

  if (numstr == ep || *ep != '\0')
    errno = EINVAL;
  else if ((ll == LLONG_MIN && errno == ERANGE) || ll < minval)
    errno = ERANGE;
  else if ((ll == LLONG_MAX && errno == ERANGE) || ll > maxval)
    errno = ERANGE;

  if (errno != 0)
    return (false);

  *res = ll;
  return (true);
}
