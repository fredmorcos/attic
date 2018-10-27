#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include "vec.h"
#include "thread.h"
#include "time.h"

#define AutoDir \
  attr((cleanup(xclosedir))) DIR *

struct lsdir {
  Thread thread;
  TimeInfo ti;

  const char *name;
  Vec *res;
  size_t nents;
  char **estr;
  int sys_errno;
} attr((designated_init));

typedef struct lsdir LsDir;

struct tdir {
  char type;                            /* D, L, F or U */
  char *filename;

  union {
    Vec children;                       /* for DIR  */
    char *target;                       /* for LINK */

    struct {                            /* for FILE */
      time_t mtime;
      unsigned long long size;
    } file;
  } info;
} attr((designated_init));

typedef struct tdir TDir;

Vec *lsdir(LsDir *const self)
  attr((warn_unused_result, nonnull));

Vec *tdir(LsDir *const self)
  attr((warn_unused_result, nonnull));

void tdir_free(TDir *const self)
  attr((nonnull));
