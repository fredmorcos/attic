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
#include <fcntl.h>
#include <limits.h>
#include <getopt.h>
#include <unistd.h>
#include <math.h>
#include <dirent.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <openssl/md5.h>

void           help(void);
void           version(void);
const char    *xrealpath(const char *const name);
unsigned char  dent_type(const struct dirent *const ent);
bool           rep(const char *const name);

enum compare_files_ret {
  COMPARE_FILES_ERR,
  COMPARE_FILES_SAME,
  COMPARE_FILES_DIFF
};

enum compare_files_ret
compare_files(const char *const fn1, const char *const fn2)
{
  assert(fn1 != NULL);
  assert(fn2 != NULL);

  enum compare_files_ret ret = COMPARE_FILES_ERR;

  const int fd1 = open(fn1, O_RDONLY);

  if (fd1 == -1) {
    warn("error: cannot open %s", fn1);
    goto finish;
  }

  const int fd2 = open(fn2, O_RDONLY);

  if (fd2 == -1) {
    warn("error: cannot open %s", fn2);
    goto finish1;
  }

  struct stat st1;

  if (fstat(fd1, &st1) == -1) {
    warn("error: cannot stat %s (fd %d)", fn1, fd1);
    goto finish2;
  }

  struct stat st2;

  if (fstat(fd2, &st2) == -1) {
    warn("error: cannot stat %s (fd %d)", fn2, fd2);
    goto finish2;
  }

  if (st1.st_size != st2.st_size) {
    ret = COMPARE_FILES_DIFF;
    goto finish2;
  }

  char *const map1 = mmap(NULL, st1.st_size, PROT_READ, MAP_PRIVATE, fd1, 0);

  if (map1 == NULL) {
    warn("error: cannot map %s (fd %d)", fn1, fd1);
    goto finish2;
  }

  char *const map2 = mmap(NULL, st2.st_size, PROT_READ, MAP_PRIVATE, fd2, 0);

  if (map2 == NULL) {
    warn("error: cannot map %s (fd %d)", fn2, fd2);
    goto finish3;
  }

  assert(st1.st_size == st2.st_size);

  ret = memcmp(map1, map2, st1.st_size) == 0 ?
    COMPARE_FILES_SAME : COMPARE_FILES_DIFF;

  (void) munmap(map2, st2.st_size);
 finish3:
  (void) munmap(map1, st1.st_size);
 finish2:
  (void) close(fd2);
 finish1:
  (void) close(fd1);
 finish:
  return ret;
}

const char *
xrealpath(const char *const name)
{
  assert(name);

  const int _errno = errno;
  char *const rp = realpath(name, NULL);
  errno = _errno;

  return rp ? rp : name;
}

unsigned char
dent_type(const struct dirent *const ent)
{
  assert(ent);

  if (ent->d_type != DT_UNKNOWN)
    return ent->d_type;

  struct stat st;

  if (stat(ent->d_name, &st) == -1)
    return DT_UNKNOWN;

  return
    S_ISBLK  (st.st_mode) ? DT_BLK  :
    S_ISCHR  (st.st_mode) ? DT_CHR  :
    S_ISDIR  (st.st_mode) ? DT_DIR  :
    S_ISFIFO (st.st_mode) ? DT_FIFO :
    S_ISLNK  (st.st_mode) ? DT_LNK  :
    S_ISREG  (st.st_mode) ? DT_REG  :
    S_ISSOCK (st.st_mode) ? DT_SOCK : DT_UNKNOWN;
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

  struct dirent *ent;

  errno = 0;
  while ((ent = readdir(dir))) {
    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    const char *const ent_rp = xrealpath(ent->d_name);

    if ((ent->d_type = dent_type(ent)) == DT_UNKNOWN) {
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
      free((char *const) ent_rp);
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
help(void)
{
  fprintf(stderr, "fdup - find duplicate files and directories\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "usage: fdup [-hv] DIRS...\n");
  fprintf(stderr, "  -h    show this help\n");
  fprintf(stderr, "  -v    show version information\n");
  /*
   * fprintf(stderr, "  -n    dry-run, do not delete any files\n");
   * fprintf(stderr, "  -l    create symlinks to duplicate files\n");
   * fprintf(stderr, "  -d    also find duplicate directories\n");
   * fprintf(stderr, "  -e    find empty files and directories\n");
   */
  fprintf(stderr, "  DIRS  directories to check for duplicates\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "see `man fdup' for more information\n");
}

void
version(void)
{
  fprintf(stderr, "fdup " FDUP_VERSION "\n");
  fprintf(stderr, "https://gitlab.com/fredmorcos/fdup\n");
  fprintf(stderr, "Fred Morcos <fred.morcos@gmail.com>\n");
  fprintf(stderr, "ISC License - see LICENSE file\n");
}

int
main(int argc, char *argv[])
{
  int ch = 0;

  while ((ch = getopt(argc, argv, ":hv")) != -1) {
    switch (ch) {
    case 'h':
      help();
      return EX_OK;
    case 'v':
      version();
      return EX_OK;
    case '?':
      errx(EX_USAGE, "error: unrecognized flag -%c, see `-h'", optopt);
    default:
      errx(EX_UNAVAILABLE, "fatal: this should not be reached, see `-h'");
    }
  }

  if (optind >= argc)
    errx(EX_USAGE, "error: no directories given, see `-h'");

  const char *const file1 = argv[optind];
  const char *const file2 = argv[optind + 1];

  switch (compare_files(file1, file2)) {
  case COMPARE_FILES_DIFF:
    puts("Files are different");
    return EX_OK;
  case COMPARE_FILES_SAME:
    puts("Files are the same");
    return EX_OK;
  case COMPARE_FILES_ERR:
    return EX_OSERR;
  }

  /*
   * MD5_CTX ctx;
   * unsigned char csum[MD5_DIGEST_LENGTH + 1];
   * if (MD5_Init(&ctx) == 0)
   *   errx(EX_SOFTWARE, "MD5 init failed");
   * if (MD5_Update(&ctx, "foobar\n", 7) == 0)
   *   errx(EX_SOFTWARE, "MD5 update failed");
   * if (MD5_Final(csum, &ctx) == 0)
   *   errx(EX_SOFTWARE, "MD5 final failed");
   * fprintf(stderr, "md5 = ");
   * for (int i = 0; i < MD5_DIGEST_LENGTH; i++)
   *   fprintf(stderr, "%x", csum[i]);
   * fprintf(stderr, "\n");
   */

  return EX_OK;
}
