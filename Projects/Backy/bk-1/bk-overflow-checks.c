#include <assert.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <sysexits.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <getopt.h>
#include <unistd.h>
#include <dirent.h>
#include <math.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>

inline bool mulof_s(const size_t x, const size_t y) __attribute__((const));

inline bool mulof_s(const size_t x, const size_t y) {
  static const size_t mulof = ((size_t) 1 << (sizeof(size_t) * 4));
  return ((x >= mulof || y >= mulof) && x > 0 && y > SIZE_MAX / x);
}

void *xrealloc(void **const p, const size_t pre,
               const size_t n, const size_t size);

void *xrealloc(void **const p, const size_t hdr,
               const size_t n, const size_t size) {
  assert(p != NULL);

  if (mulof_s(n, size) == true) {
    errno = EOVERFLOW;
    return (NULL);
  }

  const size_t bytes = n * size;

  if (bytes > SIZE_MAX - hdr) {
    errno = EOVERFLOW;
    return (NULL);
  }

  void *new_p = realloc(*p, hdr + bytes);
  return (new_p == NULL ? (NULL) : (*p = new_p));
}

struct buf {
  size_t cap;
  size_t len;
  char data[];
};

static const size_t buf_hdrsize = offsetof(struct buf, data);

void *buf_ptr(struct buf *const buf, const size_t n, const size_t size)
  __attribute__((const));
void *buf_add(struct buf **buf, const size_t n, const size_t size);

void *buf_ptr(struct buf *const buf, const size_t n, const size_t size) {
  assert(buf != NULL);

  if (mulof_s(n, size) == true) {
    errno = EOVERFLOW;
    return (NULL);
  }

  const size_t bytes = n * size;

  if (bytes > SIZE_MAX - buf_hdrsize) {
    errno = EOVERFLOW;
    return (NULL);
  }

  return ((char *const) buf + buf_hdrsize + bytes);
}

void *buf_add(struct buf **buf, const size_t n, const size_t size) {
  assert(buf != NULL);

  if (mulof_s(n, size) == true) {
    errno = EOVERFLOW;
    return (NULL);
  }

  const size_t bytes = n * size;

  struct buf *b = *buf;
  size_t ncap;
  size_t nlen;

  if (b == NULL) {
    ncap = bytes < 1024 ? 1024 : bytes;
    nlen = 0;
  } else if (b->cap - b->len < bytes) {
    if (mulof_s(b->cap, 2) == true) {
      errno = EOVERFLOW;
      return (NULL);
    }

    const size_t cap_2 = b->cap * 2;
    const size_t extra = (cap_2 - b->len) < bytes ? bytes : cap_2;

    ncap = b->cap + extra;
    nlen = b->len;
  } else {
    nlen = b->len;
    goto finish;
  }

  if (xrealloc((void **) buf, buf_hdrsize, ncap, 1) == NULL)
    return (NULL);

  b = *buf;
  b->cap = ncap;
 finish:
  b->len = nlen + bytes;
  return (buf_ptr(b, nlen, size));
}

unsigned int ndigits(unsigned long long val)
  __attribute__((const));
void ulltoa(char *str, unsigned long long val, const uint len);

unsigned int ndigits(unsigned long long val) {
  unsigned int len = 0;

  do {
    val /= 10;
    len++;
  } while (val);

  return (len);
}

void ulltoa(char *str, unsigned long long val, const uint len) {
  assert(str != NULL);
  assert(len > 0);

  double res = pow(10, len - 1);

  assert(res > 0);
  assert(res <= LLONG_MAX);

  unsigned long long div = (unsigned long long) llround(res);

  for (unsigned long long digit = 0; div > 0; div /= 10) {
    digit = val / div;
    *(str++) = (char) ('0' + digit);
    val -= digit * div;
  }
}

struct dent {
  char type;
  char *name;

  union {
    struct buf *children;               /* DIR */
    char *target;                       /* LINK */

    struct {                            /* FILE */
      time_t mtime;
      unsigned long long size;
    } file;
  } info;
};

bool tdir(const char *const name, struct dent *tree);

bool tdir(const char *const name, struct dent *tree) {
  assert(name != NULL);
  assert(tree != NULL);
  assert(tree->type == DT_DIR);

  DIR *dir = opendir(name);

  if (dir == NULL)
    return (false);

  int fd = dirfd(dir);

  if (fd == -1) {
    assert(errno != EINVAL);
    goto fail;
  }

  if (fchdir(fd) == -1) {
    assert(errno != EBADF);
    goto fail;
  }

  /* readdir() requires this */
  errno = 0;
  struct dirent *ent;

  while ((ent = readdir(dir)) != NULL) {
    struct dent *p;
    struct stat st;
    bool st_valid = false;

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &st) == -1)
        goto fail;
      else
        st_valid = true;

      if      (S_ISDIR(st.st_mode)) ent->d_type = DT_DIR;
      else if (S_ISLNK(st.st_mode)) ent->d_type = DT_LNK;
      else if (S_ISREG(st.st_mode)) ent->d_type = DT_REG;
    }

    if ((p = buf_add(&tree->info.children, 1, sizeof(struct dent))) == NULL)
      goto fail;

    (void) memset(p, 0, sizeof(struct dent));

    p->type =
      ent->d_type == DT_DIR ? DT_DIR :
      ent->d_type == DT_LNK ? DT_LNK :
      ent->d_type == DT_REG ? DT_REG : DT_UNKNOWN;

    const size_t dname_len = strlen(ent->d_name);

    if (dname_len > SIZE_MAX - 1) {
      errno = EOVERFLOW;
      goto fail;
    }

    const size_t dname_size = dname_len + 1;

    if ((p->name = malloc(dname_size)) == NULL)
      goto fail;

    (void) memcpy(p->name, ent->d_name, dname_size);

    if (ent->d_type == DT_DIR) {
      if (tdir(ent->d_name, p) == false)
        goto fail;

      if (fchdir(fd) == -1) {
        assert(errno != EBADF);
        goto fail;
      }
    } else if (ent->d_type == DT_LNK) {
      if (lstat(ent->d_name, &st) == -1)
        goto fail;

      if (st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      const size_t tsize = (size_t) st.st_size;

      if (tsize > SIZE_MAX - 1) {
        errno = EOVERFLOW;
        goto fail;
      }

      const size_t tlen = tsize + 1;

      if ((p->info.target = malloc(tlen)) == NULL)
        goto fail;

      const ssize_t llen = readlink(ent->d_name, p->info.target, tlen);
      *(p->info.target + tlen) = '\0';

      if (llen == -1)
        goto fail;

      if (llen < 0) {
        errno = ERANGE;
        goto fail;
      }

      if ((size_t) llen != tlen) {
        errno = EINVAL;
        goto fail;
      }
    } else if (ent->d_type == DT_REG) {
      if (st_valid == false) {
        if (stat(ent->d_name, &st) == -1)
          goto fail;
        st_valid = true;
      }

      if (st.st_mtim.tv_sec < 0 || st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      p->info.file.mtime = st.st_mtim.tv_sec;
      p->info.file.size = (unsigned long long) st.st_size;
    }
  }

  if (errno != 0)
    goto fail;

  if (closedir(dir) == -1)
    assert(errno != EBADF);
  return (true);

 fail: {
    int _errno = errno;
    if (closedir(dir) == -1)
      assert(errno != EBADF);
    errno = _errno;
    return (false);
  }
}

bool lsdir(const char *const name, struct buf **const buf);

bool lsdir(const char *const name, struct buf **const buf) {
  assert(name != NULL);

  DIR *dir = opendir(name);

  if (dir == NULL)
    return (false);

  int fd = dirfd(dir);

  if (fd == -1) {
    assert(errno != EINVAL);
    goto fail;
  }

  if (fchdir(fd) == -1) {
    assert(errno != EBADF);
    goto fail;
  }

  /* readdir() requires this */
  errno = 0;
  struct dirent *ent;

  while ((ent = readdir(dir)) != NULL) {
    char *p;
    struct stat st;
    bool st_valid = false;

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &st) == -1)
        goto fail;
      else
        st_valid = true;

      if      (S_ISDIR(st.st_mode)) ent->d_type = DT_DIR;
      else if (S_ISLNK(st.st_mode)) ent->d_type = DT_LNK;
      else if (S_ISREG(st.st_mode)) ent->d_type = DT_REG;
    }

    const size_t dname_len = strlen(ent->d_name);

    if (dname_len > SIZE_MAX - 1) {
      errno = EOVERFLOW;
      goto fail;
    }

    const size_t dname_size = dname_len + 1;

    if ((p = buf_add(buf, 2 + dname_size, 1)) == NULL)
      goto fail;

    *p++ =
      ent->d_type == DT_DIR ? 'D' :
      ent->d_type == DT_LNK ? 'L' :
      ent->d_type == DT_REG ? 'F' : 'U';
    *p++ = '\0';

    (void) memcpy(p, ent->d_name, dname_size);

    if (ent->d_type == DT_DIR) {
      if (lsdir(ent->d_name, buf) == false)
        goto fail;

      if (fchdir(fd) == -1) {
        assert(errno != EBADF);
        goto fail;
      }

      if ((p = buf_add(buf, 2, 1)) == NULL)
        goto fail;

      *p++ = '-';
      *p = '\0';
    } else if (ent->d_type == DT_LNK) {
      if (lstat(ent->d_name, &st) == -1)
        goto fail;

      if (st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      const size_t tsize = (size_t) st.st_size;

      if (tsize > SIZE_MAX - 1) {
        errno = EOVERFLOW;
        goto fail;
      }

      const size_t tlen = tsize + 1;

      if ((p = buf_add(buf, tlen + 1, 1)) == NULL)
        goto fail;

      const ssize_t llen = readlink(ent->d_name, p, tlen);
      *(p + tlen) = '\0';

      if (llen == -1)
        goto fail;

      if (llen < 0) {
        errno = ERANGE;
        goto fail;
      }

      if ((size_t) llen != tlen) {
        errno = EINVAL;
        goto fail;
      }
    } else if (ent->d_type == DT_REG) {
      if (st_valid == false) {
        if (stat(ent->d_name, &st) == -1)
          goto fail;
        st_valid = true;
      }

      if (st.st_mtim.tv_sec < 0 || st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      time_t mtime = st.st_mtim.tv_sec;
      long long size = st.st_size;

      const uint mtime_digits_size = ndigits((unsigned long long) mtime);
      const uint size_digits_size = ndigits((unsigned long long) size);

      if (mtime_digits_size > UINT_MAX - 1 ||
          size_digits_size > UINT_MAX - 1) {
        errno = EOVERFLOW;
        goto fail;
      }

      const uint mtime_digits_len = mtime_digits_size + 1;
      const uint size_digits_len = size_digits_size + 1;

      if (mtime_digits_len > UINT_MAX - size_digits_len) {
        errno = EOVERFLOW;
        goto fail;
      }

      const uint total_digits_len = mtime_digits_len + size_digits_len;

      if ((p = buf_add(buf, total_digits_len, 1)) == NULL)
        goto fail;

      ulltoa(p, (unsigned long long) mtime, mtime_digits_len);
      *(p + mtime_digits_len) = '\0';

      p += mtime_digits_len;

      ulltoa(p, (unsigned long long) size, size_digits_len);
      *(p + size_digits_len) = '\0';
    }
  }

  if (errno != 0)
    goto fail;

  if (closedir(dir) == -1)
    assert(errno != EBADF);
  return (true);

 fail: {
    int _errno = errno;
    if (closedir(dir) == -1)
      assert(errno != EBADF);
    errno = _errno;
    return (false);
  }
}

inline const char *af_name(const int af);
inline const char *addr_str(const int af, struct sockaddr *const addr);

void print_usage(const bool client, const bool daemon);

int main_c (const int net_fam, const char *const rootdir,
            const char *const host, const char *const port);

int main_d (const bool detach, const bool safe, const bool keep,
            const int net_fam, const char *const rootdir,
            const char *const host, const char *const port);

inline const char *af_name(const int af) {
  switch (af) {
  case AF_INET:   return "IPv4";
  case AF_INET6:  return "IPv6";
  case AF_UNSPEC: return "<Unspecified>";
  default:        return "<Other/unknown>";
  }
}

const char *addr_str(const int af, struct sockaddr *const addr) {
  assert(addr != NULL);

  const char *res = NULL;

  if (af == AF_INET) {
    static char v4_addr[INET_ADDRSTRLEN + 1];
    struct sockaddr_in sa;
    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in));
    res = inet_ntop(af, &sa.sin_addr, v4_addr, INET_ADDRSTRLEN + 1);
  } else if (af == AF_INET6) {
    static char v6_addr[INET6_ADDRSTRLEN + 1];
    struct sockaddr_in6 sa;
    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in6));
    res = inet_ntop(af, &sa.sin6_addr, v6_addr, INET6_ADDRSTRLEN + 1);
  } else {
    res = "<Unknown>";
  }

  if (res == NULL) {
    assert(errno != EAFNOSUPPORT);
    assert(errno != ENOSPC);
    res = "<Invalid>";
  }

  return res;
}

bool socksend(const int fd, const char *const buf, const size_t len);

bool socksend(const int fd, const char *const buf, const size_t len) {
  assert(fd >= 0);
  assert(buf != NULL);
  assert(len > 0);

  ssize_t send_rc = 0;
  size_t send_len = 0;

  if ((send_rc = send(fd, buf, len, 0)) == -1) {
    assert(errno != EAGAIN);
    assert(errno != EWOULDBLOCK);
    assert(errno != EBADF);
    assert(errno != EDESTADDRREQ);
    assert(errno != ENOTCONN);
    assert(errno != ENOTSOCK);
    assert(errno != EOPNOTSUPP);

    if (errno != EMSGSIZE)
      return (false);
  }

  if ((send_len = (size_t) send_rc) < len)
    return (socksend(fd, buf + send_len, len - send_len));

  assert(send_len == len);
  return (true);
}

void print_usage(const bool client, const bool daemon) {
  /* setup information about exit codes */
  static const int exitcodes[] =
    { EXIT_SUCCESS, EXIT_FAILURE, EX_USAGE, EX_OSERR };

  static const char *const exitcodes_desc[] =
    { "Success", "General failure", "Invalid usage", "System error" };

  static const size_t exitcodes_len = sizeof(exitcodes) / sizeof(int);

  /* print usage and help */
  fprintf(stderr, "Usage: bk [-h] [c|d] ...\n\n");

  if (client) {
    fprintf(stderr, "Client usage: bk c [-46] -r DIR -a HOST -p PORT\n");
    fprintf(stderr, " -h       Show this help\n");
    fprintf(stderr, " -4       IPv4 only\n");
    fprintf(stderr, " -6       IPv6 only\n");
    fprintf(stderr, " -r DIR   Sync root DIR\n");
    fprintf(stderr, " -a HOST  Connect to HOST\n");
    fprintf(stderr, " -p PORT  Connect on PORT\n");

    /* always last */
    fprintf(stderr, "\n");
  }

  if (daemon) {
    fprintf(stderr, "Daemon usage: bk d [-46dsk] -r DIR [-a HOST] -p PORT\n");
    fprintf(stderr, " -h       Show this help\n");
    fprintf(stderr, " -4       IPv4 only\n");
    fprintf(stderr, " -6       IPv6 only\n");
    fprintf(stderr, " -d       Detach\n");
    fprintf(stderr, " -s       Safe, or dry-run\n");
    fprintf(stderr, " -k       Keep running\n");
    fprintf(stderr, " -r DIR   Sync root DIR\n");
    fprintf(stderr, " -a HOST  Listen on HOST\n");
    fprintf(stderr, " -p PORT  Listen on PORT\n");

    /* always last */
    fprintf(stderr, "\n");
  }

  fprintf(stderr, "Notes:\n");
  fprintf(stderr, " Flags and arguments between [] are optional\n");
  fprintf(stderr, " Multiple flag specifications have no extra effect\n");
  fprintf(stderr, " Multiple argument specifications are not supported\n");

  /* always last */
  fprintf(stderr, "\n");

  /* print information about exit codes */
  (void) fprintf(stderr, "Exit codes:\n");

  for (size_t i = 0; i < exitcodes_len; i++)
    (void) fprintf(stderr, " %3d %s\n", exitcodes[i], exitcodes_desc[i]);
}

int main_c (const int net_fam, const char *const rootdir,
            const char *const host, const char *const port) {
  assert(net_fam == AF_INET || net_fam == AF_INET6 || net_fam == AF_UNSPEC);
  assert(rootdir != NULL);
  assert(host != NULL);
  assert(port != NULL);

  return (EXIT_SUCCESS);
}

int main_d (const bool detach, const bool safe, const bool keep,
            const int net_fam, const char *const rootdir,
            const char *const host, const char *const port) {
  assert(net_fam == AF_INET || net_fam == AF_INET6 || net_fam == AF_UNSPEC);
  assert(rootdir != NULL);
  assert(port != NULL);

  {
    char hostname[HOST_NAME_MAX + 1];

    if (gethostname(hostname, sizeof(hostname)) == -1)
      warn("Cannot get local hostname");
    else
      warnx("Hostname: %s", hostname);
  }

  struct addrinfo *ai = NULL;
  const struct addrinfo *ai_p = NULL;
  struct addrinfo hints;

  (void) memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = net_fam;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;

  const int eai = getaddrinfo(host, port, &hints, &ai);

  assert(eai != EAI_SOCKTYPE);
  assert(eai != EAI_BADFLAGS);

  if (eai != 0) {
    if (eai == EAI_SYSTEM)
      warn("Cannot get local address info (sys)");
    else
      warnx("Cannot get local address info (net): %s", gai_strerror(eai));

    freeaddrinfo(ai);
    return (EX_OSERR);
  }

  int sock = -1;
  const int opt = 1;

  for (ai_p = ai; ai_p; ai_p = ai_p->ai_next) {
    warnx("Trying to listen on %s address `%s'",
          af_name(ai_p->ai_family), addr_str(ai_p->ai_family, ai_p->ai_addr));

    sock = socket(ai_p->ai_family, ai_p->ai_socktype, ai_p->ai_protocol);

    if (sock == -1) {
      warn("  FAILED: Cannot create socket");
      continue;
    }

    /* XXX: other interesting sockopts might be:
     *  SO_KEEPALIVE: enables keep connections alive
     *  SO_LINGER:    linger on close if data present
     *  SO_SNDBUF:    set buffer size for output
     *  SO_RCVBUF:    set buffer size for input
     *  SO_SNDLOWAT:  set minimum count for output
     *  SO_RCVLOWAT:  set minimum count for input
     *  TCP_CORK (TCP_NOPUSH in FreeBSD)
     */
    (void) setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int));
    (void) setsockopt(sock, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(int));
    (void) setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int));

    if (bind(sock, ai_p->ai_addr, ai_p->ai_addrlen) == -1) {
      assert(errno != EBADF);
      assert(errno != ENOTSOCK);
      assert(errno != EINVAL);
      assert(errno != EISCONN);

      warn("  FAILED: Cannot bind socket");
      (void) close(sock);
      continue;
    }

    if (listen(sock, 0) == -1) {
      assert(errno != EBADF);
      assert(errno != EDESTADDRREQ);
      assert(errno != EINVAL);
      assert(errno != ENOTSOCK);

      warn("  FAILED: Cannot listen on socket");
      (void) close(sock);
      continue;
    }

    warnx("  SUCCESS: Listening on port %s", port);
    break;
  }

  freeaddrinfo(ai);

  if (ai_p == NULL)
    return (EX_OSERR);

  if (detach) {
    pid_t pid = fork();

    switch (pid) {
    case -1:
      warn("Cannot detach process");
      (void) close(sock);
      return (EX_OSERR);
    case 0:                             /* child */
      break;
    default:                            /* parent */
      warnx("Forked child with PID %d", pid);
      return (EXIT_SUCCESS);
    }
  }

  struct buf *dirlist = NULL;

  if (lsdir(rootdir, &dirlist) == false ||
      buf_add(&dirlist, 1, 1) == NULL) {
    warn("Cannot load dir `%s'", rootdir);
    if (dirlist != NULL)
      free(dirlist);
    (void) close(sock);
    return (EX_OSERR);
  }

  dirlist->data[dirlist->len - 1] = '\0';
  warnx("Loaded directory `%s'", rootdir);

  int csock = -1;
  struct sockaddr_storage addr;
  socklen_t addr_len;
  errno = 0;

  do {
    assert(errno != EWOULDBLOCK);
    assert(errno != EAGAIN);
    addr_len = sizeof(struct sockaddr_storage);
    csock = accept(sock, (struct sockaddr *) &addr, &addr_len);
  } while (csock == -1 && (errno == EHOSTUNREACH || errno == EHOSTDOWN ||
                           errno == ENETUNREACH  || errno == ENETDOWN  ||
                           errno == ENOPROTOOPT  || errno == ENONET    ||
                           errno == EOPNOTSUPP   || errno == EPROTO));

  if (csock == -1) {
    assert(errno != EWOULDBLOCK);
    assert(errno != EAGAIN);
    warn("Cannot accept connection");
    (void) close(sock);
    free(dirlist);
    return (EX_OSERR);
  }

  (void) close(sock);

  warnx("Accepted connection from %s address `%s'",
        af_name(addr.ss_family),
        addr_str(addr.ss_family, (struct sockaddr *) &addr));

  if (socksend(csock, dirlist->data, dirlist->len) == false) {
    warn("Cannot send directory listing");
    (void) close(csock);
    free(dirlist);
  }

  warnx("Sent directory listing");

  (void) close(csock);
  free(dirlist);

  return (EXIT_SUCCESS);
}

int main (int argc, char *argv[]) {
  bool daemon = false;

  int net_fam = AF_UNSPEC;

  bool detach = false;
  bool safe = false;
  bool keep = false;

  char *rootdir = NULL;
  char *host = NULL;
  char *port = NULL;

  {
    int ch = 0;
    const char *opts = NULL;

    if (argc <= 1) {
    usage:
      print_usage(true, true);
      return (EX_USAGE);
    } else if (!strcmp(argv[1], "-h")) {
      print_usage(true, true);
      return (EXIT_SUCCESS);
    } else if (!strcmp(argv[1], "c")) {
      opts = ":h46r:a:p:";
      daemon = false;
    } else if (!strcmp(argv[1], "d")) {
      opts = ":h46dskr:a:p:";
      daemon = true;
    } else {
      goto usage;
    }

    while ((ch = getopt(argc - 1, argv + 1, opts)) != -1) {
      switch (ch) {
      case '4':
        net_fam = net_fam == AF_INET6 ? AF_UNSPEC : AF_INET;
        break;
      case '6':
        net_fam = net_fam == AF_INET ? AF_UNSPEC : AF_INET6;
        break;
      case 'd':
        detach = true;
        break;
      case 's':
        safe = true;
        break;
      case 'k':
        keep = true;
        break;
      case 'r':
        if (rootdir)
          errx(EX_USAGE, "multiple -r arguments are not supported");
        rootdir = optarg;
        break;
      case 'a':
        if (host)
          errx(EX_USAGE, "multiple -a arguments are not supported");
        host = optarg;
        break;
      case 'p':
        if (port)
          errx(EX_USAGE, "multiple -p arguments are not supported");
        port = optarg;
        break;
      case 'h':
        print_usage(!daemon, daemon);
        return (EXIT_SUCCESS);
      case ':':
        warnx("argument -%c requires a parameter\n", optopt);
        goto ex_usage;
      case '?':
        warnx("unrecognized flag or argument -%c\n", optopt);
        goto ex_usage;
      default:
      ex_usage:
        print_usage(!daemon, daemon);
        return (EX_USAGE);
      }
    }
  }

  if (rootdir == NULL || port == NULL || (daemon == false && host == NULL)) {
    print_usage(!daemon, daemon);
    return (EX_USAGE);
  }

  return daemon ?
    main_d(detach, safe, keep, net_fam, rootdir, host, port) :
    main_c(net_fam, rootdir, host, port);
}
