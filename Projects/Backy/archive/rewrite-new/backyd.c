#include <dirent.h>
#include <errno.h>
#include <stdbool.h>
#include <sysexits.h>
#include <bsd/bsd.h>

#include "net.h"

#define autofree(f) __attribute__((cleanup (f)))

int main(const int argc, char **const argv) {
  bool  safe    = false;
  bool  detach  = false;
  char *dirname = NULL;
  char *port    = NULL;

  int autofree(net_close) cfd = -1;     /* connection socket */

  if (argc <= 1)
    errx(EX_USAGE, "No arguments given, see -h for help");

  for (int arg = 0; (arg = getopt(argc, argv, ":hsdr:p:")) != -1;) {
    if (arg == 'h') {
      warnx("usage: %s [-h -d] -r DIR -p PORT", getprogname());
      warnx("%s", "");
      warnx("options:");
      warnx(" -h           Show this help");
      warnx(" -d           Detach/daemonize");
      warnx(" -s           Do not modify files and dirs (dry run)");
      warnx(" -r DIR       Sync onto DIR");
      warnx(" -p PORT      Listen on PORT");
      return EXIT_SUCCESS;
    } else if (arg == 'd' && !detach) {
      detach = true;
    } else if (arg == 's' && !safe) {
      safe = true;
    } else if (arg == 'r') {
      if (dirname)
        errx(EX_USAGE, "Multiple -r arguments are not allowed");
      dirname = optarg;
    } else if (arg == 'p') {
      if (port)
        errx(EX_USAGE, "Multiple -p arguments are not allowed");
      port = optarg;
    } else if (arg == ':') {
      errx(EX_USAGE, "No parameter given for -%c", optopt);
    } else if (arg == '?') {
      errx(EX_USAGE, "Unrecognized argument -%c", optopt);
    }
  }

  if (!dirname)
    errx(EX_USAGE, "No root directory give, use -r");

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
      warnx("Forked child (pid %d)", pid);
      return EXIT_SUCCESS;
    }
  }

  {
    int autofree(net_close) sfd = -1;   /* server socket fd */
    struct addrinfo *ai = NULL;
    struct addrinfo autofree(net_ai_free) *l = NULL;

    if ((sfd = net_listen(port, &ai, &l)) <= 0) {
      if (sfd == 0)
        warnx("Could not bind or listen to any local address");
      else
        warnx("Could not get local address info: %s", gai_strerror(-sfd));
      return EX_OSERR;
    }

    net_ai_print("Listening on", port, ai->ai_family, ai->ai_addr);

    {
      struct sockaddr_storage addr;

      if ((cfd = net_accept(sfd, &addr)) == -1) {
        warn("Could not accept connection");
        return EX_OSERR;
      }

      net_ai_print("Accepted connection from", port,
                   ai->ai_family, (struct sockaddr *) &addr);
    }
  }

  return EXIT_SUCCESS;
}
