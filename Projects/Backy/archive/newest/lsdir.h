#pragma once

#include <stdbool.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include "vec.h"
#include "thread.h"
#include "time.h"

struct lsdir {
  struct thread thread;
  struct time_info ti;

  const char *name;
  struct vec *res;
  size_t nents;
  char **estr;
  int sys_errno;
} __attribute__((designated_init));

struct tdir {
  char type;                            /* D, L, F or U */
  char *filename;

  union {
    struct vec children;                /* for DIR */
    char *target;                       /* for LINK */

    struct {                            /* for FILE */
      time_t mtime;
      unsigned long long size;
    } file;
  } info;
} __attribute__((designated_init));

struct vec *lsdir(struct lsdir *const self)
  __attribute__((warn_unused_result, nonnull));

struct vec *tdir(struct lsdir *const self)
  __attribute__((warn_unused_result, nonnull));

void tdir_free(struct tdir *const self)
  __attribute__((nonnull));
