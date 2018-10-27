#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <limits.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#define NIL '\0'

#define ERROR_RET(t, f) {                               \
    return (Error) { .type = t, .filename = f };        \
  }

#define OK_RET ERROR_RET(EOK, NULL)

#define SAVE_ERRNO(x) {                         \
    int __errno = errno;                        \
    (void) x;                                   \
    errno = __errno;                            \
  };

#define SAVE_ERRNO_RET(x, t, f) {               \
    SAVE_ERRNO(x);                              \
    ERROR_RET(t, f);                            \
  }

#define DOT_OR_DOTDOT(s)                        \
  ((s[0] == '.') &&                             \
   (s[1] == NIL ||                              \
    (s[1] == '.' && s[2] == NIL)))

enum error_type {
  EOK = 0,
  EOPENDIR,
  EDIRFD,
  ECHDIR,
  EREADDIR,
  ESTAT,
  ESIZEOFLOW
};

struct error {
  enum error_type type;
  const char *filename;
};

typedef struct error Error;

struct string {
  char *pointer;
  size_t length;
  size_t capacity;
};

typedef struct string String;

Error dirload(const char *const dirname,
              String *const tout,
              size_t *const nelem) {
  DIR *dir;
  int  dir_fd;

  struct dirent *entry;

  assert(dirname);
  assert(tout);
  assert(nelem);

  if (!(dir = opendir(dirname)))
    ERROR_RET(EOPENDIR, dirname);

  if ((dir_fd = dirfd(dir)) == -1)
    SAVE_ERRNO_RET(closedir(dir), EDIRFD, dirname);

  if (fchdir(dir_fd) == -1)
    SAVE_ERRNO_RET(closedir(dir), ECHDIR, dirname);

  for (errno = 0; (entry = readdir(dir));) {
    if (DOT_OR_DOTDOT(entry->d_name))
      continue;

    if (entry->d_type == DT_UNKNOWN) {
      struct stat st;

      if (stat(entry->d_name, &st) == -1)
        SAVE_ERRNO_RET(closedir(dir), ESTAT, entry->d_name);

      if      (S_ISDIR(st.st_mode)) entry->d_type = DT_DIR;
      else if (S_ISLNK(st.st_mode)) entry->d_type = DT_LNK;
      else if (S_ISREG(st.st_mode)) entry->d_type = DT_REG;
    }

    {
      const size_t d_name_size = strlen(entry->d_name) + 1;
      const size_t new_length = tout->length + d_name_size;

      if (new_length > tout->capacity) {
        size_t new_capacity;

        new_capacity = tout->capacity * 2;
        new_capacity = new_capacity > 2048 ? new_capacity : 2048;
        new_capacity = new_capacity > new_length ? new_capacity : new_length;
      } else {
        (void) memcpy(&(tout->pointer[tout->length]),
                      entry->d_name, d_name_size);
      }
    }
  }

  if (errno != 0)
    SAVE_ERRNO_RET(closedir(dir), EREADDIR, dirname);

  OK_RET;
}
