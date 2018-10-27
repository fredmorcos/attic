#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "arr.h"

#define clean(x) __attribute__((cleanup (x)))

static void dir_close(DIR **dir);

enum e_dir_load {
  e_type_ok = 0,

  e_type_unknown,
  e_type_unsupported,

  e_type_errno,
  e_type_last
};

static void dir_close(DIR **dir) {
  if (*dir)
    (void) closedir(*dir);
}

int dir_load_str(struct dirent *const ent,
                 const char *const dirname,
                 struct arr *const str) {
  assert(ent);
  assert(dirname);
  assert(str);

  clean(dir_close) DIR *dir = NULL;
  struct dirent *res = NULL;
  int ret = 0;

  if (!(dir = opendir(dirname)))
    return errno;

  while ((ret = readdir_r(dir, ent, &res)) == 0) {
    if (!res)
      break;

    if (ent->d_type != DT_REG &&
        ent->d_type != DT_LNK &&
        ent->d_type != DT_DIR &&
        ent->d_type != DT_UNKNOWN) {
    }
  }

  return ret;
}
