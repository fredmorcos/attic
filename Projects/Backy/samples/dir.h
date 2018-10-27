#pragma once

#include <stdbool.h>
#include "time.h"
#include "vec.h"

struct fswarn {
  unsigned char type;
  char *name;
};

struct dir {
  const char *name;
  bool ret;

  struct vec *res;
  struct vec  warns;

  size_t nents;

  const char *err_msg;
  char *err_path;
};

void fswarn_printl(const struct vec *const l);
void fswarn_free(struct fswarn *const e);

void dir_init(struct dir *const self,
              const char *const n,
              struct vec *const res);
void dir_free(struct dir *const self);
bool dir_err(struct dir *const self,
             const char *const msg,
             const char *const path);
void dir_perr(struct dir *const self);
bool dir_load(struct dir *const self, const char *const dname);
bool dir_load_a(struct dir *const self);
