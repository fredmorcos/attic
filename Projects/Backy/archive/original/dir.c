#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <bsd/bsd.h>
#include "dir.h"
#include "util.h"

__attribute__((const))
char dtype_toc(const enum dtype t) {
  switch(t) {
  case dtype_link:  return 'L';
  case dtype_file:  return 'F';
  case dtype_dir:   return 'D';
  case daction_rem: return 'R';
  default:          return nul;
  }
}

__attribute__((const))
enum dtype dtype_fromc(const char c) {
  switch(c) {
  case 'L': return dtype_link;
  case 'F': return dtype_file;
  case 'D': return dtype_dir;
  case 'R': return daction_rem;
  default:  return nul;
  }
}

int dir_snprintf(const bool is_action_list,
                 char *const str, const size_t len,
                 const struct dir *const d) {
  if (d->type == dtype_link) {
    return snprintf(str, len, "%c%c%s%c%s",
                    dtype_toc(d->type), nul,
                    d->path,            nul,
                    d->info.target);
  } else if (d->type == dtype_file && is_action_list) {
    return snprintf(str, len, "%c%c%s%c%lld",
                    dtype_toc(d->type), nul,
                    d->path,            nul,
                    d->info.finfo.size);
  } else if (d->type == dtype_file && !is_action_list) {
    return snprintf(str, len, "%c%c%s%c%ld%c%lld",
                    dtype_toc(d->type),  nul,
                    d->path,             nul,
                    d->info.finfo.mtime, nul,
                    d->info.finfo.size);
  } else if (d->type == dtype_dir) {
    return snprintf(str, len, "%c%c%s",
                    dtype_toc(d->type), nul,
                    d->path);
  } else if (d->type == daction_rem && is_action_list) {
    return snprintf(str, len, "%c%c%s",
                    dtype_toc(d->type), nul,
                    d->path);
  } else if (d->type == daction_rem && !is_action_list) {
    return -2;
  } else {
    return -1;
  }
}

int dir_froment(const char *const path, /* parent path */
                const int d_type,
                const char *const d_name,
                struct dir *const res) {
  size_t plen = 0;                      /* full path length */

  if (d_type != DT_REG && d_type != DT_LNK && d_type != DT_DIR) {
    warnx("Skipping file (type %d): %s/%s", d_type, path, d_name);
    return -2;
  }

  if (d_type == DT_DIR && (!strcmp(d_name, ".") || !strcmp(d_name, "..")))
    return -2;

  if (asprintf(&plen, &res->path, "%s/%s", path, d_name) < 0) {
    if (errno) warn("asprintf()"); else warnx("asprintf()");
    return -1;
  }

  if (plen == 0) {
    warnx("asprintf() returned a zero string length");
    return -1;
  }

  if      (d_type == DT_LNK) res->type = dtype_link;
  else if (d_type == DT_REG) res->type = dtype_file;
  else if (d_type == DT_DIR) res->type = dtype_dir;
  else {
    warnx("Unrecognized entry type %d for %s/%s", d_type, path, d_name);
    return -1;
  }

  return 0;
}

int dir_statf(const char *const path,
              time_t *const mtime,
              long long *const size) {
  struct stat st;

  if (stat(path, &st) == -1) {
    warn("stat(): %s", path);
    return -1;
  }

  assert(st.st_mtim.tv_sec >= 0);
  assert(st.st_size >= 0);

  *mtime = st.st_mtim.tv_sec;
  *size  = st.st_size;

  assert(*mtime >= 0);
  assert(*size >= 0);

  return 0;
}

int dir_statl(const char *const path, char **const target) {
  struct stat st;
  ssize_t rl_ret = 0;                   /* readlink() return */
  size_t tg_len = 0;                    /* target length */

  if (lstat(path, &st) == -1) {
    warn("lstat(): %s", path);
    return -1;
  }

  assert(st.st_size >= 0);
  assert(st.st_size <= UINT_MAX);
  tg_len = (size_t) st.st_size + 1;

  if (!(*target = reallocarray(NULL, tg_len, sizeof(char)))) {
    warn("reallocarray(): Could not allocate link target buffer");
    return -1;
  }

  if ((rl_ret = readlink(path, *target, tg_len)) == -1) {
    warn("readlink(): %s", path);
    free(*target);
    return -1;
  } else if (rl_ret > st.st_size) {
    warn("readlink(): %s: Link name too large for buffer", path);
    free(*target);
    return -1;
  }

  assert(rl_ret >= 0);
  (*target)[rl_ret] = '\0';

  return 0;
}

void dir_free(const struct dir *const d) {
  assert(d);
  assert(d->path);

  free(d->path);

  if (d->type == dtype_link)
    free(d->info.target);
}
