#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "fs.h"

char *xrealpath(const char *const path) {
  assert(path != NULL);

  char *rp = NULL;
  int _errno = errno;

  if (!(rp = realpath(path, NULL))) {
    assert(errno != EINVAL);
    rp = strdup(path);
  }

  errno = _errno;

  return (rp);
}

DIR *xopendir(const char *const name) {
  assert(name != NULL);

  DIR *dir = NULL;

  if ((dir = opendir(name)) == NULL)
    assert(errno != ENOTDIR);

  return (dir);
}

void xclosedir(DIR **const dir) {
  assert(dir != NULL);

  if (*dir != NULL) {
    int _errno = errno;

    if (closedir(*dir) == -1)
      assert(errno != EBADF);

    errno = _errno;
  }
}

bool xchdir(DIR *const dir, const char *const name) {
  assert(dir != NULL);
  assert(name != NULL);

  int fd = -1;

  if ((fd = dirfd(dir)) == -1) {
    assert(errno != EINVAL);

    if (errno == ENOTSUP) {
      if (chdir(name) == -1) {
        assert(errno != ENOTDIR);
        return (false);
      }
      return (true);
    }
  }

  if (fchdir(fd) == -1) {
    assert(errno != EBADF);
    assert(errno != ENOTDIR);
    return (false);
  }

  return (true);
}
