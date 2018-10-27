#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <bsd/bsd.h>

bool realpath_s(const char *const p, char **res) {
  const int errno_tmp = errno;
  char *p_full = NULL;

  assert(res);
  assert(!(*res));

  if (!(p_full = realpath(p, NULL)))
    return false;

  *res = p_full;
  errno = errno_tmp;
  return true;
}

void warnc_realpath(const int errcode,
                    const char *const msg,
                    const char *const path) {
  char *path_full = NULL;

  warnc(errcode, "%s: %s",
        msg, realpath_s(path, &path_full) ? path_full : path);

  if (path_full)
    free(path_full);
}

void warn_realpath(const char *const msg, const char *const path) {
  warnc_realpath(errno, msg, path);
}
