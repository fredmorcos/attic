#include <assert.h>
#include <errno.h>
#include <dirent.h>
#include <stddef.h>
#include <stdlib.h>
#include <bsd/bsd.h>

int pc_dirent_size(const char *const path, size_t *const res) {
  long pathlen = 0;

  assert(path);
  assert(res);

  errno = 0;

  if ((pathlen = pathconf(path, _PC_NAME_MAX)) <= 0) {
    if (NAME_MAX <= 0)
      return -1;
    else
      pathlen = NAME_MAX;
  }

  *res = offsetof(struct dirent, d_name) + (size_t) pathlen + 1;

#ifdef DEBUG
  warnx("DEBUG: %s:%d", __FILE__, __LINE__);
  warnx("  Path Lengths: PATHCONF: %ld  NAME_MAX: %d", pathlen, NAME_MAX);
  warnx("                PATH_MAX: %d  MAXNAMLEN: %d", PATH_MAX, MAXNAMLEN);
#endif

  return 0;
}
