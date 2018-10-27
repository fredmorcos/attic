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
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sysexits.h>
#include <unistd.h>

int main (void) {
  const struct addrinfo hints = {
    .ai_family = AF_INET,
    .ai_socktype = SOCK_STREAM,
    .ai_flags = AI_PASSIVE
  };

  struct addrinfo *ai = NULL;

  if (getaddrinfo(NULL, "2233", &hints, &ai))
    errx(1, "getaddrinfo");

  int fd;
  struct addrinfo *p = NULL;

  for (p = ai; p; p = p->ai_next) {
    fd = socket(p->ai_family,
                p->ai_socktype,
                p->ai_protocol);

    if (fd == -1) {
      warn("socket");
      continue;
    }

    if (bind(fd, p->ai_addr, p->ai_addrlen) == -1) {
      warn("bind");
      close(fd);
      continue;
    }

    if (listen(fd, 0) == -1) {
      warn("listen");
      close(fd);
      continue;
    }
  }

  if (!p) {
    freeaddrinfo(ai);
    errx(1, "no local addr");
  }

  if (p->ai_family == AF_INET) {
    char ip[INET_ADDRSTRLEN];
    struct sockaddr_in sa;

    (void) memcpy(&sa, p->ai_addr, sizeof(struct sockaddr_in));

    if (!inet_ntop(AF_INET, &sa.sin_addr, ip, INET_ADDRSTRLEN))
      warn("inet_ntop");
    else
      warnx("listening on %.*s", INET_ADDRSTRLEN, ip);
  } else if (p->ai_family == AF_INET6) {
    char ip[INET6_ADDRSTRLEN];
    struct sockaddr_in6 sa;

    (void) memcpy(&sa, p->ai_addr, sizeof(struct sockaddr_in6));

    if (!inet_ntop(AF_INET6, &sa.sin6_addr, ip, INET6_ADDRSTRLEN))
      warn("inet_ntop");
    else
      warnx("listening on %.*s", INET6_ADDRSTRLEN, ip);
  } else {
    warnx("listening on unknown addrinfo family");
  }

  freeaddrinfo(ai);

  struct sockaddr_storage _addr;
  struct sockaddr *const addr = (struct sockaddr *) &_addr;
  socklen_t addrlen;

 retry:
  addrlen = sizeof(struct sockaddr_storage);
  const int conn_fd = accept(fd, addr, &addrlen);

  if (conn_fd == -1) {
    if (errno == ENETDOWN   || errno == ENOPROTOOPT  || errno == EPROTO ||
        errno == EHOSTDOWN  || errno == EHOSTUNREACH || errno == ENONET ||
        errno == EOPNOTSUPP || errno == ENETUNREACH)
      goto retry;
    else
      err(1, "accept");
  }

  /* TODO: clone FD and shutdown write end on one and read end on the other */

  close(fd);

  return 0;
}
