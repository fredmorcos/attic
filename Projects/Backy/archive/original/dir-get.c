#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <bsd/bsd.h>
#include "array.h"
#include "util.h"
#include "dir.h"
#include "dir-get.h"

int dir_get(const bool *const cflag,
            const char *const path,
            struct array *const res) {
  DIR *d = NULL;
  struct dirent *e = NULL;
  struct dir *tmp = NULL;
  int ret = -1;

  if (!(d = opendir(path))) {
    warn("opendir(): %s", path);
    return -1;
  }

  while ((e = readdir(d))) {
    if (!array_addn(res, 1, false, (void **) &tmp))
      goto finish;

    switch(dir_froment(path, e->d_type, e->d_name, tmp)) {
    case -1: array_revertn(res, 1); goto finish;
    case -2: array_revertn(res, 1); continue;
    default: break;
    }

    if (tmp->type == dtype_dir) {
      if (dir_get(cflag, tmp->path, res) == -1) {
        if (cflag && !(*cflag))
          warnx("Could not get subdir info for %s", tmp->path);
        goto finish;
      }
    } else if (tmp->type == dtype_file) {
      if (dir_statf(tmp->path, &tmp->info.finfo.mtime, &tmp->info.finfo.size))
        goto finish;
    } else if (tmp->type == dtype_link) {
      if (dir_statl(tmp->path, &tmp->info.target))
        goto finish;
    } else goto finish;

    if (cflag && *cflag)
      goto finish;
  }

  ret = 0;
 finish:
  if (closedir(d) == -1)
    warn("closedir(): %s", path);
  return ret;
}

void *dir_get_thread(struct dir_get_params *const params) {
  params->ret = dir_get(&params->cflag, params->path, params->res);
  return NULL;
}
