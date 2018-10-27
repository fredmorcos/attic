#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <bsd/bsd.h>
#include "array.h"
#include "dir.h"
#include "dir-get.h"
#include "dir-print.h"
#include "util.h"

char *dir_sprint(const bool is_action_list,
                 struct array *const dirs,
                 size_t *const len_) {
  const struct dir *const d = dirs->ptr;
  char *buf = NULL;
  size_t len = 0;
  size_t *line_lens = NULL;

  if (!(line_lens = reallocarray(NULL, dirs->len, sizeof(size_t)))) {
    warn("reallocarray(): Cannot allocate space for line lengths");
    return NULL;
  }

  for (size_t i = 0; i < dirs->len; i++) {
    int line_len = 0;

    if ((line_len = dir_snprintf(is_action_list, NULL, 0, &d[i])) < 0) {
      warnx(line_len == -1 ?
            "snprintf(): Cannot determine line length for %c:%s" :
            "snprintf(): Remove command now allowed in normal dirlist %c:%s",
            d[i].type, d[i].path);
      free(line_lens);
      return NULL;
    }

    line_lens[i] = (size_t) line_len;
    len += (size_t) line_len + 1;
  }

  len += 2;                             /* for the two final NULs */

  if (!(buf = reallocarray(NULL, len, sizeof(char)))) {
    warn("reallocarray(): Cannot allocate buffer for dir list");
    free(line_lens);
    return NULL;
  }

  for (size_t i = 0, cur_len = 0; i < dirs->len; i++) {
    (void) dir_snprintf(is_action_list, buf + cur_len, line_lens[i] + 1, &d[i]);
    cur_len += line_lens[i] + 1;
    buf[cur_len - 1] = nul;
  }

  buf[len - 1] = buf[len - 2] = nul;
  *len_ = len;

  free(line_lens);
  return buf;
}

char *dir_print(const bool is_action_list,
                const bool *const cflag,
                const char *const path,
                size_t *const len) {
  char *buf = NULL;
  struct array dirs;
  clock_t dir_get_cl;

  array_init(&dirs, sizeof(struct dir), (array_cb) dir_free);

  dir_get_cl = clock();
  if (dir_get(cflag, path, &dirs) != 0) {
    array_free(&dirs);
    return NULL;
  }
  element_stats("Loaded dir structure", timediff(dir_get_cl), dirs.len);

  *len = 0;
  buf = dir_sprint(is_action_list, &dirs, len);
  array_free(&dirs);
  return buf;
}

void *dir_print_thread(struct dir_print_params *const params) {
  return dir_print(params->is_action_list,
                   &params->cflag,
                   params->path,
                   params->len);
}
