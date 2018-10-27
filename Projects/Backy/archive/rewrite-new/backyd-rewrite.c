#include <stdbool.h>
#include <string.h>
#include <sysexits.h>

#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

#include <bsd/bsd.h>

void help(void);
const char *net_ai_print4(const struct sockaddr *const address,
                          char *const result);
const char *net_ai_print6(const struct sockaddr *const address,
                          char *const result);
int net_ai_print(const char *const port,
                 const int family,
                 struct sockaddr *const address,
                 const char *const prefix);
int net_ai_get(const char *const host,
               const char *const port,
               const int family,
               const int socktype,
               const int flags,
               struct addrinfo **const result);
int net_sock_listen(const char *const port,
                    struct addrinfo **const ai,
                    struct addrinfo **const ai_list,
                    int *const ai_err);

void help(void) {
  warnx("usage: %s [-h -s -d] -r DIR -p PORT", getprogname());
  warnx("%s", "");
  warnx("options:");
  warnx(" -h           Show this help");
  warnx(" -s           Do not modify files and dirs (dry run)");
  warnx(" -d           Detach/daemonize");
  warnx(" -r DIR       Sync onto DIR");
  warnx(" -p PORT      Listen on PORT");
}

const char *net_ai_print4(const struct sockaddr *const address,
                          char *const result) {
  struct sockaddr_in sa;
  (void) memcpy(&sa, address, sizeof(struct sockaddr_in));
  return inet_ntop(AF_INET, &sa.sin_addr, result, INET_ADDRSTRLEN);
}

const char *net_ai_print6(const struct sockaddr *const address,
                          char *const result) {
  struct sockaddr_in6 sa;
  (void) memcpy(&sa, address, sizeof(struct sockaddr_in6));
  return inet_ntop(AF_INET6, &sa.sin6_addr, result, INET6_ADDRSTRLEN);
}

int net_ai_print(const char *const port,
                 const int family,
                 struct sockaddr *const address,
                 const char *const prefix) {
  if (family == AF_INET) {
    char ip[INET_ADDRSTRLEN];

    if (net_ai_print4(address, ip) == NULL)
      return -1;                        /* errno is set */
    else
      warnx("%s IPv4:%.*s:%s", prefix, INET_ADDRSTRLEN, ip, port);
  } else if (family == AF_INET6) {
    char ip[INET6_ADDRSTRLEN];

    if (net_ai_print6(address, ip) == NULL)
      return -1;                        /* errno is set */
    else
      warnx("%s IPv6:%.*s:%s", prefix, INET6_ADDRSTRLEN, ip, port);
  } else {
    return -2;
  }

  return 0;
}

int net_ai_get(const char *const host,
               const char *const port,
               const int family,
               const int socktype,
               const int flags,
               struct addrinfo **const result) {
  struct addrinfo hints;

  (void) memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family   = family;
  hints.ai_socktype = socktype;
  hints.ai_flags    = flags;

  return getaddrinfo(host, port, &hints, result);
}

int net_sock_listen(const char *const port,
                    struct addrinfo **const ai,
                    struct addrinfo **const ai_list,
                    int *const ai_err) {
  int fd = 0;                           /* listening socket fd */
  struct addrinfo *p = NULL;            /* ai_list iterator */

  {
    int gai = 0;                        /* gai() return code */

    gai = net_ai_get(NULL, port, AF_UNSPEC, SOCK_STREAM, AI_PASSIVE, ai_list);

    if (gai != 0) {
      *ai_err = gai;
      return -1;                        /* ai() error, use gai_err */
    }
  }

  for (p = *ai_list; p != NULL; p = p->ai_next) {
    fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

    if (fd == -1)
      continue;

    {
      const int opt = 1;                /* socket option */

      (void) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int));
      (void) setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(int));
      (void) setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int));
    }

    if (bind(fd, p->ai_addr, p->ai_addrlen) == -1 || listen(fd, 0) == -1) {
      close(fd);
      continue;
    }

    break;
  }

  if (p == NULL)
    return -2;                          /* don't rely on errno */

  *ai = p;
  return fd;
}

int main (int argc, char **argv) {
  bool  safe    = false;
  bool  detach  = false;
  char *dirname = NULL;
  char *port    = NULL;

  if (argc <= 1)
    errx(EX_USAGE, "No arguments given, see -h for help");

  {
    const char params[] = ":hsdr:p:";

    const char fmt_dup_args_w[]  = "Duplicate -%c arguments have no effect";
    const char fmt_dup_args_e[]  = "Duplicate -%c arguments are not allowed";
    const char fmt_miss_param[]  = "Missing parameter for -%c";
    const char fmt_unknown_arg[] = "Uknown argument -%c";

    for (int arg = 0; (arg = getopt(argc, argv, params)) != -1;) {
      if (arg == 'h') {
        help();
        return 0;
      } else if (arg == 's') {
        if (safe == true)
          warnx(fmt_dup_args_w, arg);
        safe = true;
      } else if (arg == 'd') {
        if (detach == true)
          warnx(fmt_dup_args_w, arg);
        detach = true;
      } else if (arg == 'r') {
        if (dirname != NULL)
          errx(EX_USAGE, fmt_dup_args_e, arg);
        dirname = optarg;
      } else if (arg == 'p') {
        if (port != NULL)
          errx(EX_USAGE, fmt_dup_args_e, arg);
        port = optarg;
      } else if (arg == ':') {
        errx(EX_USAGE, fmt_miss_param, optopt);
      } else if (arg == '?') {
        errx(EX_USAGE, fmt_unknown_arg, optopt);
      }
    }
  }

  if (dirname == NULL)
    errx(EX_USAGE, "No root directory given, use -r");

  if (port == NULL)
    errx(EX_USAGE, "No port given, use -p");

  if (chdir(dirname) == -1)
    err(EX_CANTCREAT, "Cannot chdir into %s", dirname);

  if (detach == true) {
    pid_t pid = -1;

    switch ((pid = fork())) {
    case -1: err(EX_OSERR, "Could not fork and detach");
    case 0 : break;                     /* child */
    default:                            /* parent */
      warnx("Detached child PID: %d", pid);
      return 0;
    }
  }

  {
    int sfd = -1;                       /* server socket fd */
    int cfd = -1;                       /* connection socket fd */

    int ai_err = 0;                     /* gai() error */
    struct addrinfo *ai;                /* local address info */
    struct addrinfo *ai_list;           /* lai list */

    sfd = net_sock_listen(port, &ai, &ai_list, &ai_err);

    if (sfd == -1 || sfd == -2) {
      if (sfd == -1) {
        warnx("Could not get local address info: %s", gai_strerror(ai_err));
      } else if (sfd == -2) {
        warnx("Could not bind or listen to any local address:");

        for (struct addrinfo *p = ai_list; p != NULL; p = p->ai_next)
          switch(net_ai_print(port, p->ai_family, p->ai_addr, " ")) {
          case -1:
            warn("Error printing address information");
            continue;
          case -2:
            warnx("Error: Unsupported address family");
            continue;
          default:
            continue;
          }

        freeaddrinfo(ai_list);
      }

      return EX_OSERR;
    }

    switch(net_ai_print(port, ai->ai_family, ai->ai_addr, "Listening on")) {
    case -1:
      warn("Error printing address information");
      break;
    case -2:
      warnx("Error: Unsupported address family");
      break;
    default:
      break;
    }

    freeaddrinfo(ai_list);

    if (close(sfd) == -1)
      warn("Problem closing listening socket (FD %d)", sfd);
  }

  return EXIT_SUCCESS;
}
