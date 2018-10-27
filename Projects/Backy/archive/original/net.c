#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <bsd/bsd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include "array.h"
#include "net.h"
#include "util.h"

static const char *addrinfo_print4(const struct sockaddr *const address,
                                   char *const result) {
  struct sockaddr_in sa;
  (void) memcpy(&sa, address, sizeof(struct sockaddr_in));
  return inet_ntop(AF_INET, &sa.sin_addr, result, INET_ADDRSTRLEN);
}

static const char *addrinfo_print6(const struct sockaddr *const address,
                                   char *const result) {
  struct sockaddr_in6 sa;
  (void) memcpy(&sa, address, sizeof(struct sockaddr_in6));
  return inet_ntop(AF_INET6, &sa.sin6_addr, result, INET6_ADDRSTRLEN);
}

static void addrinfo_print(const char *const port,
                           const int family,
                           struct sockaddr *const address,
                           const char *const prefix) {
  if (family == AF_INET) {
    char ip[INET_ADDRSTRLEN];

    if (addrinfo_print4(address, ip) == NULL)
      warn("addrinfo_print():IPv4");
    else
      warnx("%s IPv4:%.*s:%s", prefix, INET_ADDRSTRLEN, ip, port);
  } else if (family == AF_INET6) {
    char ip[INET6_ADDRSTRLEN];

    if (addrinfo_print6(address, ip) == NULL)
      warn("addrinfo_print():IPv6");
    else
      warnx("%s IPv6:%.*s:%s", prefix, INET6_ADDRSTRLEN, ip, port);
  } else {
    warnx("%s <unrecognized address family>", prefix);
  }
}

static int addrinfo_get(const char *const host,
                        const char *const port,
                        const int family,
                        const int socktype,
                        const int flags,
                        struct addrinfo **result) {
  const struct addrinfo hints = {
    .ai_family   = family,
    .ai_socktype = socktype,
    .ai_flags    = flags
  };

  return getaddrinfo(host, port, &hints, result);
}

int socket_sendbuf(const int fd,
                   const char *const buf,
                   const size_t len) {
  ssize_t send_rc = 0;
  size_t send_len = 0;

#ifdef DEBUG
  char *size = humansize((double) len);
  warnx("DEBUG sendbuf(): fd %d, %s (%zd bytes)", fd, size, len);
  free(size);
#endif

  if ((send_rc = send(fd, buf, len, 0)) == -1)
    return -1;
  else if ((send_len = (size_t) send_rc) < len)
    return socket_sendbuf(fd, buf + send_len, len - send_len);

  assert(send_len == len);
  return 0;
}

int socket_recvbuf(const int fd,
                   const size_t chunk,
                   struct array *const buf) {
  char *chunkbuf = NULL;
  ssize_t recvlen = 0;

  if (!array_addn(buf, chunk, false, (void **) &chunkbuf))
    return -1;

  if ((recvlen = recv(fd, chunkbuf, chunk, 0)) == 0) {
    warnx("Connection closed unexpectedly");
    return -1;
  } else if (recvlen == -1) {
    warn("recv()");
    return -1;
  }

  assert((size_t) recvlen <= chunk);
  array_revertn(buf, chunk - (size_t) recvlen);

#ifdef DEBUG
  char *size     = humansize((double) recvlen);
  char *size_tot = humansize((double) buf->len);
  warnx("recvbuf() DEBUG: fd %d, received %s (%zd bytes), total %s (%zd bytes)",
        fd, size, (size_t) recvlen, size_tot, buf->len);
  free(size);
  free(size_tot);
#endif

  return find_nchars(chunkbuf, (size_t) recvlen, '\0', 2) == 0 ? 0 :
    socket_recvbuf(fd, chunk, buf);
}

void socket_close(const int fd,
                  const char *const desc) {
  if (close(fd) == -1)
    warn("close() on fd %d: Cannot close %s", fd, desc);
}

int socket_connect(const char *const host,
                   const char *const port) {
  int opt = 1;                          /* client socket option value */
  int fd = 0;                           /* client socket fd */

  struct addrinfo *res = NULL;          /* remote address info */

  if (addrinfo_get(host, port, AF_UNSPEC, SOCK_STREAM, 0, &res) != 0)
    return -1;

  if ((fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol)) == -1) {
    warn("Cannot create TCP socket");
    freeaddrinfo(res);
    return -1;
  }

  if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int)) == -1)
    warn("Cannot set option TCP_NODELAY on socket");

  if (connect(fd, res->ai_addr, res->ai_addrlen) == -1) {
    warn("Cannot connect to %s on port %s", host, port);
    socket_close(fd, "client socket");
    freeaddrinfo(res);
    return -1;
  }

  addrinfo_print(port, res->ai_family, res->ai_addr, "Connected to");
  freeaddrinfo(res);
  return fd;
}

int socket_accept(const int sfd) {
  int fd = 0;
  socklen_t addrlen = 0;
  struct sockaddr_storage addr;

 retry:
  addrlen = sizeof(struct sockaddr_storage);

  if ((fd = accept(sfd, (struct sockaddr *) &addr, &addrlen)) == -1 &&
      (errno == ENETDOWN   || errno == ENOPROTOOPT  || errno == EPROTO ||
       errno == EHOSTDOWN  || errno == EHOSTUNREACH || errno == ENONET ||
       errno == EOPNOTSUPP || errno == ENETUNREACH)) {
    warnx("%s, trying again...", strerror(errno));
    goto retry;
  } else if (fd == -1) {
    warn("accept()");
    return -1;
  }

  addrinfo_print("*", addr.ss_family,
                 (struct sockaddr *) &addr,
                 "Accepted connection from");
  return fd;
}

static int socket_bindlisten(const struct addrinfo *const p) {
  int fd = 0;                      /* listening socket fd */
  int opt = 1;                     /* listening socket option value */

  if ((fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) {
    warn("Cannot create TCP socket");
    return -1;
  }

  if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int)) == -1)
    warn("Cannot set option REUSEADDR on socket");

  if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int)) == -1)
    warn("Cannot set option TCP_NODELAY on socket");

  if (bind(fd, p->ai_addr, p->ai_addrlen) == -1) {
    warn("Cannot bind socket");
    goto fail;
  }

  if (listen(fd, 0) == -1) {
    warn("Cannot listen on socket");
    goto fail;
  }

  return fd;

 fail:
  socket_close(fd, "listening socket");
  return -1;
}

int socket_listen(const char *const port) {
  char hostname[HOST_NAME_MAX + 1];
  const size_t hostname_len = sizeof(hostname) / sizeof(char);

  int fd = 0;                           /* listening socket fd */

  struct addrinfo *res = NULL;          /* local address info */
  struct addrinfo *p = NULL;            /* pointer for lai list */

  if (addrinfo_get(NULL, port, AF_UNSPEC, SOCK_STREAM, AI_PASSIVE, &res) != 0)
    return -1;

  for (p = res; p != NULL; p = p->ai_next) {
    if ((fd = socket_bindlisten(p)) == -1)
      continue;
    break;
  }

  if (!p) {
    warnx("slisten(): Could not listen on any local addresses");
    freeaddrinfo(res);
    return -1;
  }

  if (gethostname(hostname, hostname_len) == -1)
    warn("gethostname()");
  else
    warnx("Hostname: %s", hostname);

  addrinfo_print(port, p->ai_family, p->ai_addr, "Listening on");
  freeaddrinfo(res);

  return fd;
}
