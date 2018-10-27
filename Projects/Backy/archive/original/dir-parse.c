#include <assert.h>
#include <stdbool.h>
#include <limits.h>
#include <bsd/bsd.h>
#include "array.h"
#include "dir.h"
#include "dir-parse.h"

int dir_parsebuf(const bool is_action_list,
                 char *const buf,
                 struct array *const res) {
  char *p = buf;
  char *str = NULL;
  struct dir *tmp = NULL;

  size_t line = 1;

  long long int strtonum_res = 0;

  while (*p) {
    if (!array_addn(res, 1, false, (void **) &tmp)) {
      warnx("Line %zd: Cannot allocate dir structure", line);
      return -1;
    }

    if (*p == dtype_toc(dtype_file) || *p == dtype_toc(dtype_dir) ||
        *p == dtype_toc(dtype_link) || *p == dtype_toc(daction_rem)) {
      if (*p == dtype_toc(daction_rem) && !is_action_list) {
        warnx("Line %zd: Found REMOVE command in file list: Not allowed", line);
        return -3;
      }

      tmp->type = dtype_fromc(*p);
      p++;

      if (*p)
        goto parse_fail;

      p++;
    } else {
      goto parse_fail;
    }

    for (str = p; *p; p++)
      continue;

    if (str == p) {
      warnx("Line %zd: Invalid path of length 0", line);
      return -1;
    }

    tmp->path = (char *) str;

    if (tmp->type == dtype_file) {
      const char *errstr = NULL;

      p++;

      if (!is_action_list) {
        for (str = p; *p; p++)
          continue;

        errstr = NULL;
        strtonum_res = strtonum(str, 0, LONG_MAX, &errstr);

        if (errstr) {
          warn("Line %zd: Cannot read mtime: value (%s) %s", line, str, errstr);
          return -1;
        }

        tmp->info.finfo.mtime = (time_t) strtonum_res;

        p++;
      }

      for (str = p; *p; p++)
        continue;

      errstr = NULL;
      tmp->info.finfo.size = strtonum(str, 0, LLONG_MAX, &errstr);

      if (errstr) {
        warn("Line %zd: Cannot read size: value (%s) %s", line, str, errstr);
        return -1;
      }
    } else if (tmp->type == dtype_link) {
      p++;

      for (str = p; *p; p++)
        continue;

      tmp->info.target = (char *) str;
    } else if (tmp->type == dtype_dir) {
    } else if (tmp->type == daction_rem) {
      assert(is_action_list);
    } else {
      goto parse_fail;
    }

    p++;
    line++;
  }

  return 0;

 parse_fail:
  warnx("Line %zd: Unrecognized char (<%c>) in dirlist buffer", line, *p);
  return -2;
}
