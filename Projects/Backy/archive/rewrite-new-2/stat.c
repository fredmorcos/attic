#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include "util.h"
#include "stat.h"

bool xfstat(const char *const path, time_t *const mtime, ll *const size) {
  assert(path);
  assert(mtime);
  assert(size);

  struct stat st;

  if (stat(path, &st) == -1)
    return false;

  assert(st.st_mtim.tv_sec >= 0);
  assert(st.st_size >= 0);

  *mtime = st.st_mtim.tv_sec;
  *size  = st.st_size;

  assert(*mtime >= 0);
  assert(*size >= 0);

  return true;
}

bool xlstatlen(const char *const path,
               size_t *const target_len,
               const char **const err_msg,
               char **const err_path) {
  assert(path);
  assert(target_len);
  assert(err_msg);
  assert(err_path);

  struct stat st;

  if (lstat(path, &st) == -1) {
    *err_msg = "Cannot stat link";
    *err_path = xrealpath(path);
    return false;
  }

  assert(st.st_size >= 0);
  assert(st.st_size <= UINT_MAX);
  *target_len = (size_t) st.st_size;

  return true;
}

bool xlstat(const char *const path,
            char *const t,
            const size_t tlen,
            const char **const err_msg,
            char **const err_path) {
  assert(path);
  assert(t);
  assert(tlen);
  assert(err_msg);
  assert(err_path);

  /* TODO The fserr() call will resolve  the link to its target rather
   * than resolve the absolute path to the link, this should be fixed.
   */

  ssize_t llen = 0;                     /* readlink() return */

  if ((llen = readlink(path, t, tlen)) == -1) {
    *err_msg = "Cannot read link target";
    *err_path = xrealpath(path);
    return false;
  }

  assert(llen >= 0);

  if ((size_t) llen != tlen) {
    *err_msg = "Link target length does not match buffer length";
    *err_path = xrealpath(path);
    return false;
  }

  *(t + tlen + 1) = NIL;

  return true;
}
