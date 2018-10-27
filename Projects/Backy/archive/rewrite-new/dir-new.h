#pragma once
#include <time.h>

enum dir_snprintf_ret {
  DIR_SNPRINTF_SUCCESS = 0,
  DIR_SNPRINTF_ERRNO   = 1,

  /* unrecognized directory type */
  DIR_SNPRINTF_INVALID_TYPE,

  /* when a remove is found a non-action_list */
  DIR_SNPRINTF_INVALID_REM,

  DIR_SNPRINTF_LAST
};

enum dir_new_link_ret {
  DIR_NEW_LINK_SUCCESS = 0,
  DIR_NEW_LINK_ERRNO   = 1,

  /* allocation failure, see errno */
  DIR_NEW_LINK_ALLOC_ERRNO,

  DIR_NEW_LINK_LAST
};

enum dir_new_dir_ret {
  DIR_NEW_DIR_SUCCESS = 0,
  DIR_NEW_DIR_ERRNO   = 1,

  /* allocation failure, see errno */
  DIR_NEW_DIR_ALLOC_ERRNO,

  DIR_NEW_DIR_LAST
};

enum dir_type {
  DIR_TYPE_LNK = 1,                     /* link */
  DIR_TYPE_FIL = 2,                     /* file */
  DIR_TYPE_DIR = 3,                     /* directory */
  DIR_TYPE_REM = 4                      /* remove (action_lists) */
};

struct dir {
  struct dir *next;

  union {
    struct {                            /* for files */
      time_t mtime;
      long long size;
    } finfo;
    char *target;                       /* for links */
  };

  enum dir_type type;
  char path[];
};

enum dir_new_link_ret
dir_new_link(size_t path_len,
             char **const path_result,
             char **const target_result,
             struct dir **const dir_result);

enum dir_new_dir_ret
dir_new_dir(size_t path_len,
            char **const path_result,
            struct dir **dir_result);

__attribute__((const))
char dir_type_tochar(const enum dir_type t);

__attribute__((const))
enum dir_type dir_type_fromchar(const char c);

int dir_snprintf(const bool is_action_list,
                 char *const str, const size_t len,
                 const struct dir *const d);
int dir_froment(const char *const path, /* parent path */
                const int d_type,
                const char *const d_name,
                struct dir *const res);
int dir_statf(const char *const path,
              time_t *const mtime,
              long long *const size);
int dir_statl(const char *const path, char **const target);
void dir_free(const struct dir *const d);
