#include <assert.h>
#include <err.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <limits.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "str.h"
#include "num.h"
#include "fs.h"
#include "vec.h"
#include "thread.h"
#include "dir.h"

bool dir_load_pre(const struct dirent *const e, struct vec *const res);
bool dir_load_h(struct dir *const self, DIR *const d, const char *const name);
void *dir_load_a_cb(struct dir *const self);

void fswarn_printl(const struct vec *const l) {
  assert(l);

  struct fswarn *p = l->ptr;

  for (size_t i = 0; i < l->len; i++, p++) {
    warnx("Warning: Skip file (type %d): %s",
          p->type, p->name ? p->name : "<Unknown filename>");
  }
}

void fswarn_free(struct fswarn *const e) {
  assert(e);
  free(e->name);
}

void dir_init(struct dir *const self,
              const char *const name,
              struct vec *const res) {
  assert(self);
  assert(name);
  assert(res);

  (void) memset(self, 0, sizeof(struct dir));

  self->name = name;
  self->ret = false;

  self->res = res;
  vec_init(&self->warns, 10, sizeof(struct fswarn), (vec_cb *) fswarn_free);

  self->nents = 0;

  {
    atomic_flag run = ATOMIC_FLAG_INIT;
    (void) atomic_flag_test_and_set(&run);
    self->run = run;
  }

  self->thread_valid = false;

  self->err_msg = NULL;
  self->err_path = NULL;
}

void dir_free(struct dir *const self) {
  assert(self);

  atomic_flag_clear(&self->run);

  if (self->thread_valid) {
    (void) pthread_join(self->thread, NULL);
    self->thread_valid = false;
  }

  vec_free(&self->warns);
}

bool dir_err(struct dir *const self,
             const char *const msg,
             const char *const path) {
  assert(self);

  if (msg) {
    assert(!self->err_msg);
    self->err_msg = msg;
  }

  if (path) {
    assert(!self->err_path);
    self->err_path = xrealpath(path);
  }

  return false;
}

void dir_perr(struct dir *const self) {
  assert(self);
  assert(self->err_msg);

  if (self->err_path) {
    warnx(" %s: %s", self->err_msg, self->err_path);
    free(self->err_path);
  } else {
    warnx(" %s", self->err_msg);
  }
}

bool dir_load_pre(const struct dirent *const e, struct vec *const res) {
  assert(e);
  assert(res);

  char *resp = NULL;
  const size_t dname_size = strlen(e->d_name) + 1;

  if (!(resp = vec_addn(res, 2 + dname_size)))
    return false;

  *resp++ =
    e->d_type == DT_DIR ? 'D' :
    e->d_type == DT_LNK ? 'L' :
    e->d_type == DT_REG ? 'F' : 'E';
  *resp++ = '\0';

  if (xstrlcpy(resp, e->d_name, dname_size) >= dname_size) {
    errno = ENOMEM;
    return false;
  }

  return true;
}

bool dir_load_h(struct dir *const self, DIR *const d, const char *const name) {
  assert(self);
  assert(d);
  assert(name);

  char *resp = NULL;

  for (struct dirent *e = NULL; errno = 0, (e = readdir(d));) {
    if (!atomic_flag_test_and_set(&self->run))
      return false;

    if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, ".."))
      continue;

    if (e->d_type == DT_UNKNOWN) {
      struct stat st;

      if (stat(e->d_name, &st) == -1)
        return dir_err(self, "Cannot stat file", e->d_name);

      if      (S_ISDIR(st.st_mode)) e->d_type = DT_DIR;
      else if (S_ISLNK(st.st_mode)) e->d_type = DT_LNK;
      else if (S_ISREG(st.st_mode)) e->d_type = DT_REG;
    }

    if ((e->d_type == DT_DIR || e->d_type == DT_LNK || e->d_type == DT_REG) &&
        !dir_load_pre(e, self->res)) {
      return dir_err(self, "Cannot extend buffer for entry info", e->d_name);
    }

    if (e->d_type == DT_DIR) {
      if (!dir_load(self, e->d_name))
        return false;

      if (!xchdir(d, &self->err_msg))
        return dir_err(self, NULL, name);

      if (!(resp = vec_addn(self->res, 2)))
        return dir_err(self, "Cannot extend buffer for dir info", e->d_name);

      *resp++ = '-';
      *resp = '\0';
    } else if (e->d_type == DT_LNK) {
      size_t tlen = 0;

      if (!xlstatlen(e->d_name, &tlen, &self->err_msg, &self->err_path))
        return false;

      assert(tlen > 0);

      if (!(resp = vec_addn(self->res, tlen + 1)))
        return dir_err(self, "Cannot extend buffer for link info", e->d_name);

      if (!xlstat(e->d_name, resp, tlen, &self->err_msg, &self->err_path))
        return false;

      *(resp + tlen) = '\0';
    } else if (e->d_type == DT_REG) {
      time_t mtime = 0;
      long long size = 0;
      uint digits = 0;

      if (!xfstat(e->d_name, &mtime, &size))
        return dir_err(self, "Cannot stat file for info", e->d_name);

      /* statfile() has already  checked whether mtime >=  0 and since
       * time_t is at most a long long, it should fit in a ull.
       */
      digits = ndigits((unsigned long long) mtime);

      if (!(resp = vec_addn(self->res, digits + 1)))
        return dir_err(self, "Cannot extend buffer for file mtime info",
                       e->d_name);

      ulltoa(resp, (unsigned long long) mtime, digits);
      *(resp + digits) = '\0';

      /* statfile() has already checked whether size >= 0 and since it
       * is a long long, it should fit in a ull.
       */
      digits = ndigits((unsigned long long) size);

      if (!(resp = vec_addn(self->res, digits + 1)))
        return dir_err(self, "Cannot extend buffer for file size info",
                       e->d_name);

      ulltoa(resp, (unsigned long long) size, digits);
      *(resp + digits) = '\0';
    } else {
      struct fswarn *w = NULL;

      if (!(w = vec_addn(&self->warns, 1)))
        return dir_err(self, "Cannot extend warnings buffer", NULL);

      *w = (struct fswarn) { .type = e->d_type, .name = xrealpath(e->d_name) };
    }

    self->nents++;
  }

  return (errno == 0);
}

bool dir_load(struct dir *const self, const char *const dname) {
  assert(self);
  assert(dname);

  attr_cleanup(xclosedir) DIR *d = NULL;

  if (!(d = opendir(dname)))
    return dir_err(self, "Cannot open directory", dname);

  if (!xchdir(d, &self->err_msg))
    return dir_err(self, NULL, dname);

  return dir_load_h(self, d, dname);
}
