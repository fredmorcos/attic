#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>
#include "lsdir.h"
#include "num.h"
#include "stat.h"
#include "fs.h"
#include "str.h"
#include "thread.h"

/* XXX: The  lsdir_rec() and tdir_rec() implementations  can seriously
 * be merged together. I just could not come up with an elegant way to
 * do so.
 */

static struct vec *tdir_rec(struct lsdir *const self,
                            struct vec *const res)
  __attribute__((warn_unused_result, nonnull));

static struct vec *lsdir_rec(struct lsdir *const self)
  __attribute__((warn_unused_result, nonnull));

static struct vec *lsdir_fail_name(struct lsdir *const self,
                                   const char *const name)
  __attribute__((warn_unused_result, nonnull));

static struct vec *lsdir_fail_errno(struct lsdir *const self)
  __attribute__((warn_unused_result, nonnull));

struct vec *tdir_rec(struct lsdir *const self,
                     struct vec *const res) {
  assert(self != NULL);
  assert(self->name != NULL);
  assert(self->res != NULL);
  assert(self->estr != NULL);
  assert(*self->estr == NULL);
  assert(self->sys_errno == 0);
  assert(res != NULL);

  struct thread *const thread = (struct thread *) self;

  __attribute__((cleanup(xclosedir))) DIR *dir = NULL;
  struct dirent *ent = NULL;

  if ((dir = xopendir(self->name)) == NULL || !xchdir(dir, self->name))
    return (lsdir_fail_name(self, self->name));

  /* readdir() requires this */
  errno = 0;

  while ((ent = readdir(dir))) {
    struct tdir *resp = NULL;
    struct { struct stat st; bool valid; } stinfo = { .valid = false };

    if (thread->thread_valid) {
      if ((errno = pthread_mutex_lock(&thread->cancel_mutex)) != 0)
        return (lsdir_fail_errno(self));

      if (thread->cancel)
        return (NULL);

      if ((errno = pthread_mutex_unlock(&thread->cancel_mutex)) != 0)
        return (lsdir_fail_errno(self));
    }

    if (!strcmp(ent->d_name, ".") ||
        !strcmp(ent->d_name, ".."))
      continue;

    {
      const size_t dname_size = strlen(ent->d_name) + 1;

      if ((resp = vec_add(res, 1)) == NULL)
        return (lsdir_fail_errno(self));

      if ((resp->filename = malloc(dname_size)) == NULL)
        return (lsdir_fail_errno(self));

      if (xstrlcpy(resp->filename, ent->d_name, dname_size) >= dname_size) {
        errno = ENOMEM;
        return (lsdir_fail_name(self, ent->d_name));
      }
    }

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &stinfo.st) == -1)
        return (lsdir_fail_name(self, ent->d_name));
      else
        stinfo.valid = true;

      if      (S_ISDIR(stinfo.st.st_mode)) ent->d_type = DT_DIR;
      else if (S_ISLNK(stinfo.st.st_mode)) ent->d_type = DT_LNK;
      else if (S_ISREG(stinfo.st.st_mode)) ent->d_type = DT_REG;
    }

    if (ent->d_type == DT_DIR) {
      resp->type = 'D';

      const char *_name = self->name;

      vec_init(&resp->info.children,
               "dir_children",
               sizeof(struct tdir),
               32,
               (vec_cb) tdir_free);

      self->name = ent->d_name;
      if (tdir_rec(self, &resp->info.children) == NULL) {
        self->name = _name;
        return (lsdir_fail_errno(self));
      }
      self->name = _name;

      if (!xchdir(dir, self->name))
        return (lsdir_fail_name(self, self->name));
    } else if (ent->d_type == DT_LNK) {
      resp->type = 'L';

      size_t tlen = 0;

      if (!xlstatlen(ent->d_name, &tlen))
        return (lsdir_fail_name(self, ent->d_name));

      assert(tlen > 0);

      if ((resp->info.target = malloc(tlen + 1)) == NULL)
        return (lsdir_fail_errno(self));

      if (!xlstat(ent->d_name, resp->info.target, tlen)) {
        /* XXX: The  xrealpath() call  under the fail_dent  label will
         * resolve  the link  to its  target rather  than resolve  the
         * absolute path to the link, this should be fixed.
         */
        return (lsdir_fail_name(self, ent->d_name));
      }
    } else if (ent->d_type == DT_REG) {
      resp->type = 'F';

      time_t mtime = 0;
      long long size = 0;

      if (stinfo.valid)
        xfinfo(&stinfo.st, &mtime, &size);
      else if (!xfstat(ent->d_name, &mtime, &size))
        return (lsdir_fail_name(self, ent->d_name));

      resp->info.file.mtime = mtime;
      resp->info.file.size = (unsigned long long) size;
    } else {
      resp->type = 'U';
    }

    self->nents++;
  }

  self->sys_errno = errno;
  return (errno == 0 ? self->res : NULL);
}

static struct vec *lsdir_rec(struct lsdir *const self) {
  assert(self != NULL);
  assert(self->name != NULL);
  assert(self->res != NULL);
  assert(self->estr != NULL);
  assert(*self->estr == NULL);
  assert(self->sys_errno == 0);

  struct thread *const thread = (struct thread *) self;

  __attribute__((cleanup(xclosedir))) DIR *dir = NULL;
  struct dirent *ent = NULL;

  if ((dir = xopendir(self->name)) == NULL || !xchdir(dir, self->name))
    return (lsdir_fail_name(self, self->name));

  /* readdir() requires this */
  errno = 0;

  while ((ent = readdir(dir))) {
    char *resp = NULL;
    struct { struct stat st; bool valid; } stinfo = { .valid = false };

    if (thread->thread_valid) {
      if ((errno = pthread_mutex_lock(&thread->cancel_mutex)) != 0)
        return (lsdir_fail_errno(self));

      if (thread->cancel)
        return (NULL);

      if ((errno = pthread_mutex_unlock(&thread->cancel_mutex)) != 0)
        return (lsdir_fail_errno(self));
    }

    if (!strcmp(ent->d_name, ".") ||
        !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &stinfo.st) == -1)
        return (lsdir_fail_name(self, ent->d_name));
      else
        stinfo.valid = true;

      if      (S_ISDIR(stinfo.st.st_mode)) ent->d_type = DT_DIR;
      else if (S_ISLNK(stinfo.st.st_mode)) ent->d_type = DT_LNK;
      else if (S_ISREG(stinfo.st.st_mode)) ent->d_type = DT_REG;
    }

    {
      const size_t dname_size = strlen(ent->d_name) + 1;

      if ((resp = vec_add(self->res, 2 + dname_size)) == NULL)
        return (lsdir_fail_name(self, ent->d_name));

      *resp++ =
        ent->d_type == DT_DIR ? 'D' :
        ent->d_type == DT_LNK ? 'L' :
        ent->d_type == DT_REG ? 'F' : 'U';
      *resp++ = '\0';

      if (xstrlcpy(resp, ent->d_name, dname_size) >= dname_size) {
        errno = ENOMEM;
        return (lsdir_fail_name(self, ent->d_name));
      }
    }

    if (ent->d_type == DT_DIR) {
      const char *_name = self->name;

      self->name = ent->d_name;
      if (lsdir_rec(self) == NULL) {
        self->name = _name;
        return (lsdir_fail_errno(self));
      }
      self->name = _name;

      if (!xchdir(dir, self->name))
        return (lsdir_fail_name(self, self->name));

      if ((resp = vec_add(self->res, 2)) == NULL)
        return (lsdir_fail_errno(self));

      *resp++ = '-';
      *resp = '\0';
    } else if (ent->d_type == DT_LNK) {
      size_t tlen = 0;

      if (!xlstatlen(ent->d_name, &tlen))
        return (lsdir_fail_name(self, ent->d_name));

      assert(tlen > 0);

      if ((resp = vec_add(self->res, tlen + 1)) == NULL)
        return (lsdir_fail_errno(self));

      if (!xlstat(ent->d_name, resp, tlen)) {
        /* XXX: The  xrealpath() call  under the fail_dent  label will
         * resolve  the link  to its  target rather  than resolve  the
         * absolute path to the link, this should be fixed.
         */
        return (lsdir_fail_name(self, ent->d_name));
      }
    } else if (ent->d_type == DT_REG) {
      time_t mtime = 0;
      long long size = 0;
      uint digits = 0;

      if (stinfo.valid)
        xfinfo(&stinfo.st, &mtime, &size);
      else if (!xfstat(ent->d_name, &mtime, &size))
        return (lsdir_fail_name(self, ent->d_name));

      /* xfstat() has  already checked  whether mtime  >= 0  and since
       * time_t is at  most a long long, it should  fit in an unsigned
       * long long.
       */
      digits = ndigits((unsigned long long) mtime);

      if ((resp = vec_add(self->res, digits + 1)) == NULL)
        return (lsdir_fail_errno(self));

      ulltoa(resp, (unsigned long long) mtime, digits);
      *(resp + digits) = '\0';

      /* xfstat() has already  checked whether size >= 0  and since it
       * is a long long, it should fit in an unsigned long long.
       */
      digits = ndigits((unsigned long long) size);

      if ((resp = vec_add(self->res, digits + 1)) == NULL)
        return (lsdir_fail_errno(self));

      ulltoa(resp, (unsigned long long) size, digits);
      *(resp + digits) = '\0';
    }

    self->nents++;
  }

  self->sys_errno = errno;
  return (errno == 0 ? self->res : NULL);
}

struct vec *lsdir(struct lsdir *const self) {
  assert(self != NULL);

  struct vec *res = NULL;

  ti_now(&self->ti, true);

  if ((res = lsdir_rec(self)) != NULL) {
    /* XXX: this variable is volatile  because the line below (*resp =
     * nul) is,  for some reason,  optimized out  on GCC and  I cannot
     * figure out why.
     */
#if defined(__GNUC__) && !defined(__clang__)
    volatile
#endif
      char *resp = NULL;

    if ((resp = vec_add(self->res, 1)) == NULL) {
      self->sys_errno = errno;
      res = NULL;
    } else {
      *resp = '\0';
    }
  }

  ti_diff(&self->ti);

  return (res);
}

struct vec *tdir(struct lsdir *const self) {
  assert(self != NULL);

  struct vec *res = NULL;

  ti_now(&self->ti, true);
  res = tdir_rec(self, self->res);
  ti_diff(&self->ti);

  return (res);
}

void tdir_free(struct tdir *const self) {
  assert(self != NULL);

  if (self->type == 'D')
    vec_free(&self->info.children);
  else if (self->type == 'L' && self->info.target != NULL)
    free(self->info.target);

  if (self->filename != NULL)
    free(self->filename);
}

inline struct vec *lsdir_fail_name(struct lsdir *const self,
                                   const char *const name) {
  assert(self != NULL);
  assert(self->estr != NULL);
  assert(*self->estr == NULL);
  assert(name != NULL);
  *self->estr = xrealpath(name);
  return (lsdir_fail_errno(self));
}

inline struct vec *lsdir_fail_errno(struct lsdir *const self) {
  assert(self != NULL);
  self->sys_errno = errno;
  return (NULL);
}
