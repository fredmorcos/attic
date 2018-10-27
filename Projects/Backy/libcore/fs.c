/*
 * Copyright (c) 2015-2016, Fred Morcos <fred.morcos@gmail.com>
 *
 * Permission to  use, copy,  modify, and/or distribute  this software
 * for any  purpose with  or without fee  is hereby  granted, provided
 * that the above  copyright notice and this  permission notice appear
 * in all copies.
 *
 * THE  SOFTWARE IS  PROVIDED "AS  IS"  AND THE  AUTHOR DISCLAIMS  ALL
 * WARRANTIES  WITH  REGARD TO  THIS  SOFTWARE  INCLUDING ALL  IMPLIED
 * WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO EVENT  SHALL THE
 * AUTHOR   BE  LIABLE   FOR   ANY  SPECIAL,   DIRECT,  INDIRECT,   OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF  USE,  DATA  OR  PROFITS,  WHETHER IN  AN  ACTION  OF  CONTRACT,
 * NEGLIGENCE  OR  OTHER  TORTIOUS  ACTION,   ARISING  OUT  OF  OR  IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include "fs.h"

char *xrealpath(const char *const p) {
  assert(p != NULL);

  char *rp = NULL;
  int tmp_errno = errno;

  if (!(rp = realpath(p, NULL)))
    rp = strdup(p);

  errno = tmp_errno;
  return rp;
}

bool xchdir(DIR *const d, const char **const err_msg) {
  assert(d != NULL);
  assert(err_msg != NULL);

  int fd = -1;

  if ((fd = dirfd(d)) == -1) {
    *err_msg = "Could not get directory file descriptor";
    return false;
  }

  if (fchdir(fd) == -1) {
    *err_msg = "Could not change into directory";
    return false;
  }

  return true;
}

void xclosedir(DIR **d) {
  assert(d != NULL);

  if (*d) {
    int tmp_errno = errno;
    (void) closedir(*d);
    errno = tmp_errno;
  }
}

bool xfstat(const char *const path,
            time_t *const mtime,
            long long *const size) {
  assert(path != NULL);
  assert(mtime != NULL);
  assert(size != NULL);

  struct stat st;

  if (stat(path, &st) == -1)
    return false;

  assert(st.st_mtim.tv_sec >= 0);
  assert(st.st_size >= 0);

  *mtime = st.st_mtim.tv_sec;
  *size  = st.st_size;

  assert(*mtime >= 0);
  assert(*size >= 0);

  return true;
}

bool xlstatlen(const char *const path,
               size_t *const target_len,
               const char **const err_msg,
               char **const err_path) {
  assert(path != NULL);
  assert(target_len != NULL);
  assert(err_msg != NULL);
  assert(err_path != NULL);

  struct stat st;

  if (lstat(path, &st) == -1) {
    *err_msg = "Cannot stat link";
    *err_path = xrealpath(path);
    return false;
  }

  assert(st.st_size >= 0);
  assert(st.st_size <= UINT_MAX);
  *target_len = (size_t) st.st_size;

  return true;
}

bool xlstat(const char *const path,
            char *const t,
            const size_t tlen,
            const char **const err_msg,
            char **const err_path) {
  assert(path != NULL);
  assert(t != NULL);
  assert(tlen > 0);
  assert(err_msg != NULL);
  assert(err_path != NULL);

  /*
   * XXX: The  xrealpath() call  will resolve the  link to  its target
   * rather than resolve the absolute path to the link, this should be
   * fixed.
   */

  ssize_t llen = 0;                     /* readlink() return */

  if ((llen = readlink(path, t, tlen)) == -1) {
    *err_msg = "Cannot read link target";
    *err_path = xrealpath(path);
    return false;
  }

  assert(llen >= 0);

  if ((size_t) llen != tlen) {
    *err_msg = "Link target length does not match buffer length";
    *err_path = xrealpath(path);
    return false;
  }

  *(t + tlen + 1) = '\0';

  return true;
}
