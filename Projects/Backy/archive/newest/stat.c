#include <assert.h>
#include <limits.h>
#include <unistd.h>
#include "stat.h"

void xfinfo(const struct stat *const st,
            time_t *const mtime,
            long long *const size) {
  assert(st != NULL);
  assert(mtime != NULL);
  assert(size != NULL);

  assert(st->st_mtim.tv_sec >= 0);
  assert(st->st_size >= 0);

  *mtime = st->st_mtim.tv_sec;
  *size  = st->st_size;

  assert(*mtime >= 0);
  assert(*size >= 0);
}

bool xfstat(const char *const path,
            time_t *const mtime,
            long long *const size) {
  assert(path != NULL);
  assert(mtime != NULL);
  assert(size != NULL);

  struct stat st;

  if (stat(path, &st) == -1)
    return (false);

  xfinfo(&st, mtime, size);

  return (true);
}

bool xlstatlen(const char *const path, size_t *const tlen) {
  assert(path != NULL);
  assert(tlen != NULL);

  struct stat st;

  if (lstat(path, &st) == -1)
    return (false);

  assert(st.st_size >= 0);
  assert(st.st_size <= UINT_MAX);

  *tlen = (size_t) st.st_size;

  return (true);
}

bool xlstat(const char *const path, char *const target, const size_t tlen) {
  assert(path != NULL);
  assert(target != NULL);
  assert(tlen > 0);

  /* readlink() return */
  ssize_t llen = 0;

  if ((llen = readlink(path, target, tlen)) == -1)
    return (false);

  assert(llen >= 0);

  if ((size_t) llen != tlen)
    return (false);

  *(target + tlen) = '\0';

  return (true);
}
