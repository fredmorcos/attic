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

struct dent {
  char type;
  char *name;

  union {
    char *target;                       /* LINK */

    struct {                            /* FILE */
      time_t mtime;
      unsigned long long size;
    } file;

    struct {                            /* DIR */
      struct dent *children;
      size_t children_len;
    } dir;
  } info;
};

bool lsdir(const char *const name, FILE *const buf);
bool lsdir_tree(const char *const name, FILE *const buf);

bool lsdir(const char *const name, FILE *const buf) {
  assert(name != NULL);

  DIR *const dir = opendir(name);

  if (dir == NULL)
    return (false);

  const int fd = dirfd(dir);

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
    char type = 'U';
    struct stat st;
    bool st_valid = false;

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &st) == -1)
        goto fail;
      else
        st_valid = true;

      if      (S_ISDIR(st.st_mode)) type = 'D';
      else if (S_ISLNK(st.st_mode)) type = 'L';
      else if (S_ISREG(st.st_mode)) type = 'F';
    }

    if (fprintf(buf, "%c%c%s%c", type, '\0', ent->d_name, '\0') < 0) {
      errno = ENOMEM;
      goto fail;
    }

    if (type == 'D') {
      if (lsdir(ent->d_name, buf) == false)
        goto fail;

      if (fchdir(fd) == -1) {
        assert(errno != EBADF);
        goto fail;
      }

      if (fwrite("-", 1, 2, buf) < 2) {
        errno = ENOMEM;
        goto fail;
      }
    } else if (type == 'L') {
      char tstr[PATH_MAX + 1];
      const ssize_t rl = readlink(ent->d_name, tstr, PATH_MAX + 1);

      if (rl == -1)
        goto fail;

      tstr[rl] = '\0';

      if (fprintf(buf, "%s%c", tstr, '\0') < 0) {
        errno = ENOMEM;
        goto fail;
      }
    } else if (type == 'F') {
      if (st_valid == false && stat(ent->d_name, &st) == -1)
        goto fail;

      if (st.st_mtim.tv_sec < 0 || st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      if (fprintf(buf, "%zu%c%zu%c",
                  (size_t) st.st_mtim.tv_sec, '\0',
                  (size_t) st.st_size, '\0') < 0) {
        errno = ENOMEM;
        goto fail;
      }
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

bool lsdir_tree(const char *const name, FILE *const buf) {
  assert(name != NULL);

  DIR *const dir = opendir(name);

  if (dir == NULL)
    return (false);

  const int fd = dirfd(dir);

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
    char type = 'U';
    struct stat st;
    bool st_valid = false;
    struct dent dent;

    if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
      continue;

    if (ent->d_type == DT_UNKNOWN) {
      if (stat(ent->d_name, &st) == -1)
        goto fail;
      else
        st_valid = true;

      if      (S_ISDIR(st.st_mode)) type = 'D';
      else if (S_ISLNK(st.st_mode)) type = 'L';
      else if (S_ISREG(st.st_mode)) type = 'F';
    }

    const size_t dname_size = strlen(ent->d_name) + 1;

    dent.type = type;
    dent.name = malloc(dname_size);

    if (dent.name == NULL)
      goto fail;

    (void) memcpy(dent.name, ent->d_name, dname_size);

    if (type == 'D') {
      FILE *children_f = open_memstream
        ((char **) &dent.info.dir.children, &dent.info.dir.children_len);

      if (children_f == NULL) {
        free(dent.name);
        goto fail;
      }

      if (lsdir_tree(ent->d_name, children_f) == false) {
        int _errno = errno;
        (void) fclose(children_f);
        errno = _errno;
        free(dent.name);
        free(dent.info.dir.children);
        goto fail;
      }

      (void) fclose(children_f);

      if (fchdir(fd) == -1) {
        assert(errno != EBADF);
        free(dent.name);
        free(dent.info.dir.children);
        goto fail;
      }

      if (fwrite(&dent, sizeof(struct dent), 1, buf) < sizeof(struct dent)) {
        errno = ENOMEM;
        free(dent.name);
        free(dent.info.dir.children);
        goto fail;
      }
    } else if (type == 'L') {
      char tstr[PATH_MAX + 1];
      const ssize_t rl = readlink(ent->d_name, tstr, PATH_MAX + 1);

      if (rl == -1) {
        free(dent.name);
        goto fail;
      }

      tstr[rl] = '\0';

      dent.info.target = malloc((size_t) rl + 1);

      if (dent.info.target == NULL) {
        free(dent.name);
        goto fail;
      }

      (void) memcpy(dent.info.target, tstr, (size_t) rl + 1);

      if (fwrite(&dent, sizeof(struct dent), 1, buf) < sizeof(struct dent)) {
        errno = ENOMEM;
        free(dent.name);
        free(dent.info.target);
        goto fail;
      }
    } else if (type == 'F') {
      /* XXX: continue here */
      if (st_valid == false && stat(ent->d_name, &st) == -1)
        goto fail;

      if (st.st_mtim.tv_sec < 0 || st.st_size < 0) {
        errno = ERANGE;
        goto fail;
      }

      if (fprintf(buf, "%zu%c%zu%c",
                  (size_t) st.st_mtim.tv_sec, '\0',
                  (size_t) st.st_size, '\0') < 0) {
        errno = ENOMEM;
        goto fail;
      }
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

const char *af_name(const int af) __attribute__((const));
const char *addr_str(const int af, struct sockaddr *const addr);

const char *af_name(const int af) {
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

void print_usage(const bool client, const bool daemon);
int main_c (const int net_fam, const char *const rootdir,
            const char *const host, const char *const port);
int main_d (const bool detach, const bool safe, const bool keep,
            const int net_fam, const char *const rootdir,
            const char *const host, const char *const port);

void print_usage(const bool client, const bool daemon) {
  /* setup information about exit codes */
  const int exitcodes[] =
    { EXIT_SUCCESS, EXIT_FAILURE, EX_USAGE, EX_OSERR };

  const char *const exitcodes_desc[] =
    { "Success", "General failure", "Invalid usage", "System error" };

  const size_t exitcodes_len = sizeof(exitcodes) / sizeof(int);

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

  struct dent *dirtree = NULL;
  size_t dirtree_len = 0;
  FILE *dirtree_f = NULL;

  dirtree_f = open_memstream((char **) &dirtree, &dirtree_len);

  if (dirtree_f == NULL) {
    warn("Cannot allocate memory for dir tree");
    return (EX_OSERR);
  }

  if (lsdir_tree(rootdir, dirtree_f) == false ||
      fwrite("\0", 1, 1, dirtree_f) < 1) {
    warn("Cannot load dir listing for `%s'", rootdir);
    (void) fclose(dirtree_f);
    free(dirtree);
    return (EX_OSERR);
  }

  (void) fclose(dirtree_f);
  warnx("Loaded directory `%s'", rootdir);

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
          af_name(ai_p->ai_family),
          addr_str(ai_p->ai_family, ai_p->ai_addr));

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
     *  TCP_CORK (TCP_NOPUSH on FreeBSD)
     *
     * These are more useful to be applied on the client socket rather than the
     * listening socket.
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

  char *dirlist = NULL;
  size_t dirlist_len = 0;
  FILE *dirlist_f = NULL;

  dirlist_f = open_memstream(&dirlist, &dirlist_len);

  if (dirlist_f == NULL) {
    warn("Cannot allocate memory for directory listing");
    return (EX_OSERR);
  }

  if (lsdir(rootdir, dirlist_f) == false ||
      fwrite("\0", 1, 1, dirlist_f) < 1) {
    warn("Cannot load directory listing for `%s'", rootdir);
    (void) close(sock);
    (void) fclose(dirlist_f);
    free(dirlist);
    return (EX_OSERR);
  }

  (void) fclose(dirlist_f);
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

  if (socksend(csock, dirlist, dirlist_len) == false) {
    warn("Cannot send directory listing");
    (void) close(csock);
    free(dirlist);
    return (EX_OSERR);
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
    } else if (strcmp(argv[1], "-h") == 0) {
      print_usage(true, true);
      return (EXIT_SUCCESS);
    } else if (strcmp(argv[1], "c") == 0) {
      opts = ":h46r:a:p:";
      daemon = false;
    } else if (strcmp(argv[1], "d") == 0) {
      opts = ":h46dskr:a:p:";
      daemon = true;
    } else {
      goto usage;
    }

    while ((ch = getopt(argc - 1, argv + 1, opts)) != -1) {
      if      (ch == '4') net_fam = net_fam == AF_INET6 ? AF_UNSPEC : AF_INET;
      else if (ch == '6') net_fam = net_fam == AF_INET ? AF_UNSPEC : AF_INET6;
      else if (ch == 'd') detach = true;
      else if (ch == 's') safe = true;
      else if (ch == 'k') keep = true;
      else if (ch == 'r') {
        if (rootdir != NULL) errx(EX_USAGE, "multiple -r arguments are not supported");
        rootdir = optarg;
      } else if (ch == 'a') {
        if (host != NULL) errx(EX_USAGE, "multiple -a arguments are not supported");
        host = optarg;
      } else if (ch == 'p') {
        if (port != NULL) errx(EX_USAGE, "multiple -p arguments are not supported");
        port = optarg;
      } else if (ch == 'h') {
        print_usage(!daemon, daemon);
        return (EXIT_SUCCESS);
      } else if (ch == ':') {
        warnx("argument -%c requires a parameter\n", optopt);
        goto ex_usage;
      } else if (ch == '?') {
        warnx("unrecognized flag or argument -%c\n", optopt);
        goto ex_usage;
      } else {
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
