#include <arpa/inet.h>
#include <assert.h>
#include <err.h>
#include <errno.h>
#include <getopt.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>
#include "net.h"

/* options */
#define DETACH 1
#define SAFE   2
#define KEEP   4

static int sig = -1;

static void sighdlr(const int _sig);
static void usage(const int ec);

static void sighdlr(const int _sig) {
  sig = _sig;
}

static void usage(const int ec) {
  errx(ec, "usage: bkd [-h46dsk] -r dir [-a host] -p port");
}

int main (const int argc, char *const argv[]) {
  int opts = 0x0;
  int fam = AF_UNSPEC;

  char *dir = NULL;
  char *host = NULL;
  char *port = NULL;

  if (argc <= 1)
    usage(EX_USAGE);

  do {
    const int c = getopt(argc, argv, ":h46dskr:a:p:");

    if (c == -1)
      break;

    switch (c) {
    case 'h':
      usage(EX_OK);
      break;
    case '4':
      fam = AF_INET;
      break;
    case '6':
      fam = AF_INET6;
      break;
    case 'd':
      opts |= DETACH;
      break;
    case 's':
      opts |= SAFE;
      break;
    case 'k':
      opts |= KEEP;
      break;
    case 'r':
      dir = optarg;
      break;
    case 'a':
      host = optarg;
      break;
    case 'p':
      port = optarg;
      break;
    case '?':
      warnx("unknown -%c flag", optopt);
      usage(EX_USAGE);
      break;
    case ':':
      warnx("missing -%c arg", optopt);
      usage(EX_USAGE);
      break;
    }
  } while (1);

  if (!dir) {
    warnx("no -r arg provided");
    usage(EX_USAGE);
  }

  if (!port) {
    warnx("no -p arg provided");
    usage(EX_USAGE);
  }

  /* setup sig handler */
  const struct sigaction sa = {
    .sa_handler = sighdlr
  };

  if (sigaction(SIGTERM, &sa, NULL) == -1)
    warn("sigaction SIGTERM");

  if (sigaction(SIGINT, &sa, NULL) == -1)
    warn("sigaction SIGINT");

  /* print local hostname */
  const char *const hname = hostname();

  if (!hname)
    warn("gethostname");
  else
    warnx("hostname: %s", hname);

  struct addrinfo *ai = NULL;
  struct addrinfo *p = NULL;
  const int tai = tcp_addrinfo(fam, host, port, &ai);
  int lfd = -1;

  if (tai)
    ai_err(tai, EX_NOHOST, "getaddrinfo");

  for (p = ai; p; p = p->ai_next) {
    lfd = tcp_listen(p);

    if (lfd < 0)
      ai_perr("skip", p, port, tcp_error(lfd));
    else
      break;
  }

  if (!p) {
    warnx("cannot use any local addr");
    freeaddrinfo(ai);
    return EX_NOHOST;
  }

  ai_pmsg("listen", p, port);
  fam = p->ai_family;
  freeaddrinfo(ai);

  if (opts & DETACH) {
    const pid_t pid = fork();

    switch (pid) {
    case -1:
      err(EX_OSERR, "fork");
    case 0:                             /* child */
      break;
    default:                            /* parent */
      warnx("forked pid %d", pid);
      return EX_OK;
    }
  }

  warnx("waiting for connections...");

  struct sockaddr_storage _addr;
  struct sockaddr *const addr = (struct sockaddr *) &_addr;
  socklen_t alen;

 retry:
  alen = sizeof(struct sockaddr_storage);
  const int cfd = accept(lfd, addr, &alen);

  if (cfd == -1) {
    if (errno == EINTR) {
      if (sig == SIGTERM) {
        warnx("accept: terminated");
        goto term;
      } else if (sig == SIGINT) {
        warnx("accept: interrupted");
        goto term;
      }

      warn("accept: try again: received signal %d", sig);
      sig = -1;
      goto retry;

    term:
      (void) close(lfd);
      return EX_OK;
    }

    if (tcp_retryaccept(errno)) {
      warn("accept: try again");
      goto retry;
    } else {
      warn("accept");
      (void) close(lfd);
      return EX_NOHOST;
    }
  }

  (void) close(lfd);

  sock_enopt(cfd, TCP_NODELAY);
  sa_pmsg("accept", fam, addr);

  int ni = 0;
  const char *const chname = nameinfo(addr, alen, &ni);

  if (!chname)
    ai_warn(ni, "getnameinfo");
  else
    warnx("client: %s", chname);

  (void) close(cfd);

  return EX_OK;
}
