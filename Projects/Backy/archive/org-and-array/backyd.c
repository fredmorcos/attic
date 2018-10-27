#include <bsd/bsd.h>
#include <stdbool.h>
#include <sysexits.h>

#include <errno.h>
#include <limits.h>
#include <unistd.h>

#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>

#define autovar(x) __attribute__((cleanup(x)))

void net_socket_close(int *fd);
void net_addrinfo_free(struct addrinfo **ai);

void net_socket_close(int *fd) {
  if (fd && *fd >= 0) {
    if (close(*fd) == -1)
      warn("Cannot close fd %d", *fd);
  }
}

void net_addrinfo_free(struct addrinfo **ai) {
  if (ai)
    freeaddrinfo(*ai);
}

int main (int argc, char **const argv) {
  bool  safe    = false;
  bool  detach  = false;
  bool  keep    = false;
  char *dirname = NULL;
  char *addr    = NULL;
  char *port    = NULL;

  if (argc <= 1)
    errx(EX_USAGE, "No argument given, see -h for help");

  {
    const char fmt_extra_arg[] = "Multiple -%c arguments have no extra effect";
    const char fmt_inval_arg[] = "Multiple -%c arguments are not allowed";

    for (int arg = 0; (arg = getopt(argc, argv, ":hsdkr:a:p:")) != -1;) {
      if (arg == 'h') {
        warnx("usage: %s [-h -d -k] -r DIR -a ADDR -p PORT", getprogname());
        warnx("%s", "");
        warnx("options:");
        warnx(" -h           Show this help");
        warnx(" -s           Do not modify files and dirs (dry run)");
        warnx(" -d           Detach/daemonize");
        warnx(" -k           Keep running after sync is over");
        warnx(" -r DIR       Sync onto DIR");
        warnx(" -a ADDR      Listen on ADDR (optional)");
        warnx(" -p PORT      Listen on PORT");
        return EXIT_SUCCESS;
      } else if (arg == 's') {
        if (safe)
          warnx(fmt_extra_arg, arg);
        safe = true;
      } else if (arg == 'd') {
        if (detach)
          warnx(fmt_extra_arg, arg);
        detach = true;
      } else if (arg == 'k') {
        if (detach)
          warnx(fmt_extra_arg, arg);
        keep = true;
      } else if (arg == 'r') {
        if (dirname)
          errx(EX_USAGE, fmt_inval_arg, arg);
        dirname = optarg;
      } else if (arg == 'a') {
        if (addr)
          errx(EX_USAGE, fmt_inval_arg, arg);
        addr = optarg;
      } else if (arg == 'p') {
        if (port)
          errx(EX_USAGE, fmt_inval_arg, arg);
        port = optarg;
      } else if (arg == ':') {
        errx(EX_USAGE, "No parameter given for -%c", optopt);
      } else if (arg == '?') {
        errx(EX_USAGE, "Unrecognized argument -%c", optopt);
      }
    }
  }

  if (!dirname)
    errx(EX_USAGE, "No root directory given, use -r");

  if (!port)
    errx(EX_USAGE, "No port given, use -p");

  if (chdir(dirname) == -1)
    err(EX_CANTCREAT, "Cannot change dir into %s", dirname);

  if (detach) {
    pid_t pid = -1;

    switch ((pid = fork())) {
    case -1: err(EX_OSERR, "Cannot detach process");
    case  0: break;                     /* child */
    default:                            /* parent */
      warnx("Child detached (pid %d)", pid);
      return EXIT_SUCCESS;
    }
  }

 main_loop:
  {
    autovar(net_socket_close) int cfd = -1;

    {
      autovar(net_socket_close) int sfd = -1;

      {
        const int opt = 1;

        int gai_errno = 0;

        autovar(net_addrinfo_free) struct addrinfo *res = NULL;

        struct addrinfo *p = NULL;
        struct addrinfo  hints;

        const char *addrstr = addr ? addr : "*";

        (void) memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family   = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags    = AI_PASSIVE;

        gai_errno = getaddrinfo(addr, port, &hints, &res);

        if (gai_errno != 0) {
          warnx("Cannot get local address info on %s:%s: %s",
                addrstr, port, gai_strerror(gai_errno));
          return EX_OSERR;
        }

        for (p = res; p; p = p->ai_next) {
          sfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

          if (sfd == -1) {
            warn("Cannot creates TCP socket to listen on %s:%s", addrstr, port);
            continue;
          }

          if (setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int)) == -1)
            warn("Cannot set REUSEADDR on socket");

          if (setsockopt(sfd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(int)) == -1)
            warn("Cannot set REUSEPORT on socket");

          if (setsockopt(sfd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int)) == -1)
            warn("Cannot set TCP_NODELAY on socket");

          if (bind(sfd, p->ai_addr, p->ai_addrlen) == -1) {
            warn("Cannot bind TCP socket to %s:%s", addrstr, port);
            net_socket_close(&sfd);
            continue;
          }

          if (listen(sfd, 0) == -1) {
            warn("Cannot listen on %s:%s", addrstr, port);
            net_socket_close(&sfd);
            continue;
          }

          break;
        }

        if (!p) {
          warnx("Could not listen on %s:%s", addrstr, port);
          return EX_OSERR;
        }

        if (p->ai_family == AF_INET) {
          const int addr_len = INET_ADDRSTRLEN;

          char ip[INET_ADDRSTRLEN];

          struct sockaddr_in sa;
          const size_t sa_len = sizeof(sa);

          (void) memcpy(&sa, p->ai_addr, sa_len);

          if (inet_ntop(AF_INET, &sa.sin_addr, ip, INET_ADDRSTRLEN) == NULL)
            warn("Cannot print address, guess: IPv4 %s:%s", addrstr, port);
          else
            warnx("Listening on IPv4 %.*s:%s (%s)", addr_len, ip, port, addrstr);
        } else if (p->ai_family == AF_INET6) {
          const int addr_len = INET6_ADDRSTRLEN;

          char ip[INET6_ADDRSTRLEN];

          struct sockaddr_in6 sa;
          const size_t sa_len = sizeof(sa);

          (void) memcpy(&sa, p->ai_addr, sa_len);

          if (inet_ntop(AF_INET6, &sa.sin6_addr, ip, INET6_ADDRSTRLEN) == NULL)
            warn("Cannot print address, guess: IPv6 %s:%s", addrstr, port);
          else
            warnx("Listening on IPv6 %.*s:%s (%s)", addr_len, ip, port, addrstr);
        } else {
          warnx("Cannot print listen address due to unknown address family");
        }
      }

      {
        char hostname[HOST_NAME_MAX + 1];
        const size_t hostname_len = sizeof(hostname) - 1;

        if (gethostname(hostname, hostname_len) == -1)
          warn("Cannot get local hostname");
        else
          warnx("Local hostname is %s", hostname);
      }

      {
        struct sockaddr_storage addr;
        socklen_t addr_len = 0;

      retry:
        addr_len = sizeof(struct sockaddr_storage);

        if ((cfd = accept(sfd, (struct sockaddr *) &addr, &addr_len)) == -1 &&
            (errno == ENETDOWN   || errno == ENOPROTOOPT  || errno == EPROTO ||
             errno == EHOSTDOWN  || errno == EHOSTUNREACH || errno == ENONET ||
             errno == EOPNOTSUPP || errno == ENETUNREACH)) {
          warn("Cannot accept connection, will try again");
          goto retry;
        } else if (cfd == -1) {
          warn("Cannot accept connection");
          return EX_OSERR;
        }

        /* print accepted connection from */
      }
    }
  }

  if (keep)
    goto main_loop;

  return EXIT_SUCCESS;
}
