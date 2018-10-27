#include <arpa/inet.h>
#include <assert.h>
#include <err.h>
#include <errno.h>
#include <getopt.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>
#include "net.h"

static void usage(const int code);

static void usage(const int code) {
  errx(code, "Usage: bkc [-h46] -r dir -a host -p port");
}

int main (const int argc, char *const argv[]) {
  int fam = AF_UNSPEC;

  char *dir = NULL;
  char *host = NULL;
  char *port = NULL;

  if (argc <= 1)
    usage(EX_USAGE);

  do {
    const int c = getopt(argc, argv, ":h46r:a:p:");

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

  if (!host) {
    warnx("no -a arg provided");
    usage(EX_USAGE);
  }

  if (!port) {
    warnx("no -p arg provided");
    usage(EX_USAGE);
  }

  /* print local hostname */
  const char *const hname = hostname();

  if (!hname)
    warn("gethostname");
  else
    warnx("hostname: %s", hname);

  struct addrinfo *ai = NULL;
  struct addrinfo *p = NULL;
  const int tai = tcp_addrinfo(fam, host, port, &ai);
  int cfd = -1;

  if (tai)
    ai_err(tai, EX_NOHOST, "getaddrinfo");

  for (p = ai; p; p = p->ai_next) {
    cfd = tcp_connect(p);

    if (cfd < 0)
      ai_perr("skip", p, port, tcp_error(cfd));
    else
      break;
  }

  if (!p) {
    warnx("cannot use any local addr to connect");
    freeaddrinfo(ai);
    return EX_NOHOST;
  }

  ai_pmsg("connect", p, port);
  fam = p->ai_family;
  freeaddrinfo(ai);
  sock_enopt(cfd, TCP_NODELAY);

  (void) close(cfd);

  return EX_OK;
}
