#include <sys/file.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/xattr.h>

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

#include <attr/xattr.h>
#include <openssl/sha.h>

typedef unsigned char uchar;

#define EOPEN 1                       /* open() */
#define ELOCK 2                       /* fcntl() lock */
#define ESTAT 3                       /* stat() */
#define ENOTREGF 4                    /* not S_ISREG() */
#define ESIZE 5                       /* file or attr size */
#define EMMAP 6                       /* mmap() */
#define EMTIMEATTR 7                  /* fgetxattr() mtime */
#define ECSUMATTR 8                   /* fgetxattr() csum */
#define EFUTURE 9                     /* csum mtime > mtime */
#define ECORRUPTED 10                 /* file corrupted */

#define CSUMLEN ((size_t) SHA256_DIGEST_LENGTH)
#define MTIMELEN (sizeof(time_t))

static const char ATTRCSUM[] = "user.sha256sum";
static const char ATTRMTIME[] = "user.sha256mtime";

static int sig = -1;

void sighdlr(const int _sig);
int  usage(const int rv);

int fd_open(const char *const fn);
int fd_close(const int fd, const int rv);
int fd_info(const int fd, size_t *const size, time_t *const mtime);
int fd_csum(const int fd, const size_t size, uchar *const csum);
int fd_check(const char *const fn);

void sighdlr(const int _sig)
{
  sig = _sig;
}

int usage(const int rv)
{
  warnx("usage: shatag [-h] [-f file | -d dir]");
  return rv;
}

int fd_open(const char *const fn)
{
  const int fd = open(fn, O_RDONLY);

  if (fd == -1)
    return -EOPEN;

  struct flock lock = {
    .l_type = F_RDLCK,
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0,
    .l_pid = 0
  };

  if (fcntl(fd, F_SETLKW, &lock) == -1) {
    const int _errno = errno;
    (void) close(fd);
    errno = _errno;
    return -ELOCK;
  }

  return fd;
}

int fd_close(const int fd, const int rv)
{
  const int _errno = errno;

  struct flock lock = {
    .l_type = F_UNLCK,
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0,
    .l_pid = 0
  };

  (void) fcntl(fd, F_SETLKW, &lock);
  (void) close(fd);

  errno = _errno;

  return rv;
}

int fd_info(const int fd, size_t *const size, time_t *const mtime)
{
  struct stat st;

  if (fstat(fd, &st) == -1)
    return -ESTAT;

  if (!S_ISREG(st.st_mode))
    return -ENOTREGF;

  if (st.st_size < 0)
    return -ESIZE;

  *size = (size_t) st.st_size;
  *mtime = st.st_mtim.tv_sec;

  return 0;
}

int fd_csum(const int fd, const size_t size, uchar *const csum)
{
  const uchar *const data =
    mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);

  if (!data)
    return -EMMAP;

  (void) SHA256(data, size, csum);

  return 0;
}

int fd_check(const char *const fn)
{
  size_t size;                          /* file size from stat() */
  time_t cur_mtime;                     /* current mtime */
  time_t old_mtime;                     /* stored checksum mtime */
  uchar cur_csum[CSUMLEN];              /* current checksum */
  uchar old_csum[CSUMLEN];              /* stored checksum */
  int rv;
  ssize_t attr_rv;

  const int fd = fd_open(fn);

  if (fd < 0)
    return fd;

  rv = fd_info(fd, &size, &cur_mtime);

  if (rv < 0)
    return fd_close(fd, rv);

  attr_rv = fgetxattr(fd, ATTRMTIME, &old_mtime, MTIMELEN);

  if (attr_rv < 0) {
    if (errno == ENOATTR || errno == ERANGE)
      goto invalid_mtime;
    else
      return fd_close(fd, -EMTIMEATTR);
  }

  if ((size_t) attr_rv < MTIMELEN) {
  invalid_mtime:
    /* BUG: cur_mtime may be = TIME_T_MIN, also, invalidating the mtime this way
     * really is a hack until i figure out how to deal with the clusterfuck of
     * combinations of possible return conditions
     */
    old_mtime = cur_mtime - 1;
  }

  attr_rv = fgetxattr(fd, ATTRCSUM, old_csum, CSUMLEN);

  if (attr_rv < 0) {
    if (errno == ENOATTR || errno == ERANGE)
      goto invalid_csum;
    else
      return fd_close(fd, -ECSUMATTR);
  }

  if ((size_t) attr_rv < CSUMLEN) {
  invalid_csum:
    /* BUG: cur_mtime may be = TIME_T_MIN, also, invalidating the checksum this
     * way really is a hack until i figure out how to deal with the clusterfuck
     * of combinations of possible return conditions
     */
    old_mtime = cur_mtime - 1;
  }

  /* something has gone wrong because the last stored checksum mtime is newer
   * than the current file mtime, return -EFUTURE
   */
  if (old_mtime > cur_mtime)
    return fd_close(fd, -EFUTURE);

  rv = fd_csum(fd, size, cur_csum);

  if (rv < 0)
    return fd_close(fd, rv);

  /* the file has been modified since the last checksum was calculated, store
   * the new checksum and mtime
   */
  if (old_mtime < cur_mtime) {
    rv = fsetxattr(fd, ATTRMTIME, &cur_mtime, MTIMELEN, 0);

    if (rv < 0)
      return fd_close(fd, rv);

    rv = fsetxattr(fd, ATTRCSUM, cur_csum, CSUMLEN, 0);

    if (rv < 0)
      return fd_close(fd, rv);

    return fd_close(fd, 0);
  }

  /* the file has not been modified since the last checksum was calculated,
   * compare the checksums and if they do not match, then silent corruption
   * occurred. this of course assumes that metadata checksums are enabled as
   * part of the filesystem, otherwise nothing is guaranteed
   */
  if (old_mtime == cur_mtime) {
    rv = memcmp(old_csum, cur_csum, CSUMLEN);

    if (rv)
      return fd_close(fd, -ECORRUPTED);

    return fd_close(fd, 0);
  }

  return fd_close(fd, 0);
}

int main (const int argc, char *const argv[]) {
  const char *file = NULL;
  const char *dir = NULL;

  if (argc <= 1)
    return usage(EX_USAGE);

  do {
    const int c = getopt(argc, argv, ":hf:d:");

    if (c == -1)
      break;
    else if (c == 'h')
      return usage(0);
    else if (c == 'f')
      file = optarg;
    else if (c == 'd')
      dir = optarg;
    else if (c == '?')
      errx(usage(EX_USAGE), "unknown -%c flag", optopt);
    else if (c == ':')
      errx(usage(EX_USAGE), "missing -%c arg", optopt);
  } while (1);

  if (!file && !dir)
    errx(usage(EX_USAGE), "no -f or -d arg provided");

  if (file && dir)
    errx(usage(EX_USAGE), "-f and -d are mutually exclusive");

  /* setup sig handler */
  const struct sigaction sa = {
    .sa_handler = sighdlr
  };

  if (sigaction(SIGTERM, &sa, NULL) == -1)
    warn("sigaction SIGTERM");

  if (sigaction(SIGINT, &sa, NULL) == -1)
    warn("sigaction SIGINT");

  if (file) {
    const int rv = fd_check(file);

    if (rv < 0)
      err(EX_OSFILE, "error %d in %s", rv, file);
  }

  if (dir) {
    /* TODO */
  }

  return 0;
}
