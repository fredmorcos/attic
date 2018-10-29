#include <assert.h>
#include <err.h>
#include <errno.h>
#include <sysexits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>
#include <getopt.h>
#include <unistd.h>
#include <math.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

void help(const bool header);
char *xrealpath(char *const name);
bool dirent_type(struct dirent *const ent);
bool rep(const char *const name);

char *
xrealpath(char *const name)
{
  assert(name);

  const int _errno = errno;
  char *const rp = realpath(name, NULL);
  errno = _errno;

  return rp ? rp : name;
}

bool
dirent_type(struct dirent *const ent)
{
  assert(ent);

  if (ent->d_type != DT_UNKNOWN)
    return true;

  struct stat st;

  if (stat(ent->d_name, &st) == -1)
    return false;

  ent->d_type =
    S_ISBLK  (st.st_mode) ? DT_BLK  :
    S_ISCHR  (st.st_mode) ? DT_CHR  :
    S_ISDIR  (st.st_mode) ? DT_DIR  :
    S_ISFIFO (st.st_mode) ? DT_FIFO :
    S_ISLNK  (st.st_mode) ? DT_LNK  :
    S_ISREG  (st.st_mode) ? DT_REG  :
    S_ISSOCK (st.st_mode) ? DT_SOCK : DT_UNKNOWN;

  return true;
}

bool
rep(const char *const name)
{
  assert(name);

  DIR *const dir = opendir(name);

  if (!dir)
    return false;

  const int fd = dirfd(dir);

  if (fd == -1) {
    assert(errno != EINVAL);
    goto fail;
  }

  if (fchdir(fd) == -1) {
    assert(errno != EBADF);
    goto fail;
  }

  /* readdir() requires this? */
  errno = 0;

  struct dirent *ent;

  while ((ent = readdir(dir))) {
    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    char *const ent_rp = xrealpath(ent->d_name);

    if (!dirent_type(ent) || ent->d_type == DT_UNKNOWN) {
      warn("skip %s: cannot determine type", ent_rp);
    } else if (ent->d_type == DT_DIR) {
      if (!rep(ent->d_name))
        goto fail;

      if (fchdir(fd) == -1) {
        assert(errno != EBADF);
        goto fail;
      }
    } else if (ent->d_type == DT_REG) {
    } else {
      warnx("skip %s: unsupported type", ent_rp);
    }

    if (ent_rp != ent->d_name) {
      assert(ent_rp);
      free(ent_rp);
    }
  }

  if (errno)
    goto fail;

  if (closedir(dir) == -1)
    assert(errno != EBADF);

  return true;

 fail: {
    const int _errno = errno;
    if (closedir(dir) == -1)
      assert(errno != EBADF);
    errno = _errno;
    return false;
  }
}

void
help(const bool header)
{
  if (header)
    fprintf(stderr, "rep - replicate a directory with hard-links to files\n");

  fprintf(stderr, "\n");
  fprintf(stderr, "usage: rep [-h] SRC\n");
  fprintf(stderr, "  -h    show this help\n");
  fprintf(stderr, "  SRC   source dir to replicate\n");
}

int
main(int argc, char *argv[])
{
  char *srcdir = NULL;

  int ch = 0;

  while ((ch = getopt(argc, argv, ":h")) != -1) {
    switch (ch) {
    case 'h':
      help(true);
      return EX_OK;
    case '?':
      warnx("error: unrecognized flag -%c", optopt);
      help(false);
      return EX_USAGE;
    default:
      warnx("fatal: this is not supposed to be reached");
      help(false);
      return EX_UNAVAILABLE;
    }
  }

  if (optind >= argc) {
    warnx("error: no source dir given\n");
    help(false);
    return EX_USAGE;
  }

  srcdir = argv[optind];

  if (optind + 1 < argc) {
    warnx("error: too many arguments given\n");
    help(false);
    return EX_USAGE;
  }

  assert(srcdir);

  if (!rep(srcdir)) {
    warn("error: cannot replicate %s", srcdir);
    return EX_OSERR;
  }

  return EX_OK;
}
