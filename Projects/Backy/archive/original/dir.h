enum dtype {
  dtype_link  = 1,
  dtype_file  = 2,
  dtype_dir   = 3,
  daction_rem = 4                       /* only for diractions */
};

struct dir {
  enum dtype type;
  char *path;

  union {
    struct {
      time_t mtime;
      long long size;
    } finfo;
    char *target;
  } info;
};

__attribute__((const))
char dtype_toc(const enum dtype t);
__attribute__((const))
enum dtype dtype_fromc(const char c);
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
