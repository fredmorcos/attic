#include "net.h"

#include <errno.h>
#include <bsd/bsd.h>

#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

void net_close(int *const fd) {
  if (*fd != -1)
    (void) close(*fd);
}

int net_listen(const char *const port,
               struct addrinfo **const ai,
               struct addrinfo **const l) {
  int fd = 0;                           /* listening socket fd */

  {
    int gai = 0;
    struct addrinfo hints;

    (void) memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;

    if ((gai = getaddrinfo(NULL, port, &hints, l)) != 0)
      return gai < 0 ? gai : -gai;      /* use gai_strerror() */
  }

  {
    struct addrinfo *p = NULL;          /* ai_list iterator */

    for (p = *l; p != NULL; p = p->ai_next) {
      if ((fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1)
        continue;

      {
        const int opt = 1;              /* socket option */

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

    if (!p)
      return 0;                         /* don't rely on errno */

    *ai = p;
  }

  return fd;
}

int net_accept(const int sfd, struct sockaddr_storage *const addr) {
  int fd = -1;
  socklen_t addrlen = 0;

 retry:
  addrlen = sizeof(struct sockaddr_storage);

  if ((fd = accept(sfd, (struct sockaddr *) addr, &addrlen)) == -1 &&
      (errno == ENETDOWN   || errno == ENOPROTOOPT  || errno == EPROTO ||
       errno == EHOSTDOWN  || errno == EHOSTUNREACH || errno == ENONET ||
       errno == EOPNOTSUPP || errno == ENETUNREACH))
    goto retry;
  else
    return fd;
}

void net_ai_free(struct addrinfo **ai) {
  if (*ai)
    freeaddrinfo(*ai);
}

void net_ai_print(const char *const prefix, const char *const port,
                  const int fam, struct sockaddr *const addr) {
  if (fam == AF_INET) {
    char ip[INET_ADDRSTRLEN];
    struct sockaddr_in sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in));

    if (!inet_ntop(AF_INET, &sa.sin_addr, ip, INET_ADDRSTRLEN))
      warn("%s: Could not print address information", prefix);
    else
      warnx("%s IPv4:%.*s:%s", prefix, INET_ADDRSTRLEN, ip, port);
  } else if (fam == AF_INET6) {
    char ip[INET6_ADDRSTRLEN];
    struct sockaddr_in6 sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in6));

    if (!inet_ntop(AF_INET6, &sa.sin6_addr, ip, INET6_ADDRSTRLEN))
      warn("%s: Could not print address information", prefix);
    else
      warnx("%s IPv6:%.*s:%s", prefix, INET6_ADDRSTRLEN, ip, port);
  } else {
    warnx("%s: Unknown address family", prefix);
  }
}
