#include <stdio.h>
#include <dirent.h>
#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <sysexits.h>
#include <unistd.h>
#include <stddef.h>

#include <bsd/bsd.h>

#include "arr.h"

#define autoref(x) __attribute__((cleanup (x)))

void dirent_free(struct dirent **ent);
void dir_close(DIR **dir);

void dirent_free(struct dirent **ent) {
  if (*ent)
    free(*ent);
}

void dir_close(DIR **dir) {
  if (*dir && closedir(*dir) == -1)
    warn("Cannot close dir");
}

int main(int argc, char *argv[]) {
  autoref(dir_close)   DIR *dir = NULL;
  autoref(dirent_free) struct dirent *ent = NULL;
  struct dirent *res = NULL;
  int ret = 0;

  long path_len = 0;
  size_t dirent_size = 0;

  if (argc <= 1) {
    warnx("No parameter given");
    return EX_USAGE;
  }

  if ((path_len = pathconf(argv[1], _PC_NAME_MAX)) == -1) {
    if ((path_len = NAME_MAX) <= 0) {
      path_len = 255;
      warnx("Could not determine path length at all, assuming 255");
    }
  }

  assert(path_len > 0);
  dirent_size = offsetof(struct dirent, d_name) + (size_t) path_len + 1;

  if (!(ent = reallocarray(NULL, 1, dirent_size))) {
    warn("Cannot allocate space for dirent");
    return EX_OSERR;
  }

  if (!(dir = opendir(argv[1]))) {
    warn("Cannot open directory %s", argv[1]);
    return EX_OSERR;
  }

  while ((ret = readdir_r(dir, ent, &res)) == 0) {
    if (!res)
      break;
    warnx("%s", ent->d_name);
  }

  if (ret) {
    warnc(ret, "Cannot read dir");
    return EX_OSERR;
  }

  autoref(arr_free) struct arr a;
  arr_init(&a, sizeof(char), NULL);

  if (arr_extn(&a, 100) != 0)
    warn("Cannot extend array");

  return EXIT_SUCCESS;
}
