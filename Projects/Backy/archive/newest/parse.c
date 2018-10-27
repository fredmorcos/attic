#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <inttypes.h>
#include "parse.h"
#include "vec.h"
#include "lsdir.h"
#include "num.h"

/**
 *  0 success
 * -1 error with errno (ie, vec_add() ENOMEM)
 * -2 general parsing error without errno
 * -3 parsing error with errno (ie, strtonum() ERANGE)
 */
int lsdir_parse(char **const buf, struct vec *const res) {
  assert(buf != NULL);
  assert(res != NULL);

  char *p = *buf;

  while (*p != '\0') {
    struct tdir *dir = NULL;

    if (*p == '-') {
      p++;
      *buf = p;
      return (0);
    }

    if ((dir = vec_add(res, 1)) == NULL)
      return (-1);

    if (*p == 'D' || *p == 'L' || *p == 'F' || *p == 'U') dir->type = *p;
    else return (-2);

    p++;

    if (*p != '\0')
      return (-2);

    p++;

    {                                   /* parse filename */
      for (dir->filename = p; *p != '\0'; p++)
        continue;

      if (dir->filename == p)
        return (-2);

      p++;
    }

    if (dir->type == 'D') {
      int ret = 0;

      vec_init(&dir->info.children,
               "dir_children",
               sizeof(struct tdir),
               64,
               (vec_cb) tdir_free);

      if ((ret = lsdir_parse(&p, &dir->info.children)) < 0)
        return (ret);

      if (*p != '\0')
        return (-2);

      p++;
    } else if (dir->type == 'L') {
      for (dir->info.target = p; *p != '\0'; p++)
        continue;

      if (dir->info.target == p)
        return (-1);

      p++;
    } else if (dir->type == 'F') {
      const char *str = NULL;

      long long num = 0;

      for (str = p; *p != '\0'; p++)
        continue;

      if (!strtonum(str, 0, LLONG_MAX, &num))
        return (-3);

      dir->info.file.mtime = num;

      p++;

      for (str = p; *p != '\0'; p++)
        continue;

      if (!strtonum(str, 0, LLONG_MAX, &num))
        return (-3);

      dir->info.file.size = (unsigned long long) num;

      p++;
    } else if (dir->type == 'U') {
    } else {
      assert(false);
    }

    p++;
  }

  *buf = p;
  return (0);
}
