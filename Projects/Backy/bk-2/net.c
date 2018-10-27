#include <assert.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <netdb.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include "net.h"

char *hostname(void) {
  static char hn[HOST_NAME_MAX + 1];
  return gethostname(hn, sizeof(hn)) == -1 ? NULL : hn;
}

char *nameinfo(const struct sockaddr *const addr,
               const socklen_t alen,
               int *const ret) {
  assert(addr);
  assert(alen > 0);
  assert(ret);

  static char hn[NI_MAXHOST + 1];
  const int _ret = getnameinfo(addr, alen,
                              hn, sizeof(hn),
                              NULL, 0, NI_NAMEREQD);

  if (_ret != 0) {
    assert(_ret != EAI_BADFLAGS);
    assert(_ret != EAI_FAMILY);
    assert(_ret != EAI_OVERFLOW);
    return NULL;
  }

  return hn;
}

int tcp_addrinfo(const int fam,
                 const char *const host,
                 const char *const port,
                 struct addrinfo **const ai) {
  assert(fam == AF_INET ||
         fam == AF_INET6 ||
         fam == AF_UNSPEC);
  assert(port);
  assert(ai);
  assert(!(*ai));

  const struct addrinfo hints = {
    .ai_family = fam,
    .ai_socktype = SOCK_STREAM,
    .ai_flags = AI_PASSIVE
  };

  const int rv = getaddrinfo(host, port, &hints, ai);

  assert(rv != EAI_BADFLAGS);
  assert(rv != EAI_SOCKTYPE);
  assert((!rv && *ai) || (rv && !(*ai)));

  return rv;
}

void sock_enopt(const int fd, const int optname) {
  static const int opt = 1;
  (void) setsockopt(fd, SOL_SOCKET, optname, &opt, sizeof(int));
}

int sock_close(const int fd, const int ret) {
  const int _errno = errno;
  (void) close(fd);
  errno = _errno;
  return ret;
}

const char *tcp_error(const int rv) {
  return
    rv == NET_ESOCKET ? "socket" :
    rv == NET_EBIND ? "bind" :
    rv == NET_ELISTEN ? "listen" :
    rv == NET_ECONNECT ? "connect" :
    NULL;
}

int tcp_listen(const struct addrinfo *const ai) {
  const int fd = socket(ai->ai_family,
                        ai->ai_socktype,
                        ai->ai_protocol);

  if (fd == -1)
    return NET_ESOCKET;

  sock_enopt(fd, SO_REUSEADDR);
  sock_enopt(fd, SO_REUSEPORT);

  if (bind(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
    assert(errno != EBADF);
    assert(errno != ENOTSOCK);
    assert(errno != EINVAL);
    assert(errno != EISCONN);
    return sock_close(fd, NET_EBIND);
  }

  if (listen(fd, 0) == -1) {
    assert(errno != EBADF);
    assert(errno != EDESTADDRREQ);
    assert(errno != EINVAL);
    assert(errno != ENOTSOCK);
    return sock_close(fd, NET_ELISTEN);
  }

  return fd;
}

int tcp_connect(const struct addrinfo *const ai) {
  const int fd = socket(ai->ai_family,
                        ai->ai_socktype,
                        ai->ai_protocol);

  if (fd == -1)
    return NET_ESOCKET;

  if (connect(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
    assert(errno != EALREADY);
    assert(errno != EBADF);
    assert(errno != EISCONN);
    assert(errno != ENOTSOCK);
    assert(errno != EINVAL);
    assert(errno != EOPNOTSUPP);
    return sock_close(fd, NET_ECONNECT);
  }

  return fd;
}

int tcp_retryaccept(const int _errno) {
  return
    _errno == ENETDOWN   || _errno == ENOPROTOOPT  || _errno == EPROTO ||
    _errno == EHOSTDOWN  || _errno == EHOSTUNREACH || _errno == ENONET ||
    _errno == EOPNOTSUPP || _errno == ENETUNREACH;
}

const char *addrfam(const int fam) {
  assert(fam == AF_INET || fam == AF_INET6);
  return fam == AF_INET ? "IPv4" : "IPv6";
}

int addrlen(const int fam) {
  assert(fam == AF_INET || fam == AF_INET6);
  return fam == AF_INET ? INET_ADDRSTRLEN : INET6_ADDRSTRLEN;
}

const char *addrip(const int fam, const struct sockaddr *const addr) {
  const int _errno = errno;
  const char *res = NULL;

  if (fam == AF_INET) {
    static char ip[INET_ADDRSTRLEN];
    struct sockaddr_in sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in));

    if (!inet_ntop(AF_INET, &sa.sin_addr, ip, INET_ADDRSTRLEN)) {
      assert(errno != EAFNOSUPPORT);
      assert(errno != ENOSPC);
    }

    res = ip;
  } else if (fam == AF_INET6) {
    static char ip6[INET6_ADDRSTRLEN];
    struct sockaddr_in6 sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in6));

    if (!inet_ntop(AF_INET6, &sa.sin6_addr, ip6, INET6_ADDRSTRLEN)) {
      assert(errno != EAFNOSUPPORT);
      assert(errno != ENOSPC);
    }

    res = ip6;
  }

  errno = _errno;
  return res;
}

void ai_warn(const int errval, const char *const msg) {
  if (errval == EAI_SYSTEM)
    warn("%s", msg);
  else
    warnx("%s: %s", msg, gai_strerror(errval));
}

void ai_err(const int errval, const int ecode, const char *const msg) {
  if (errval == EAI_SYSTEM)
    err(ecode, "%s", msg);
  else
    errx(ecode, "%s: %s", msg, gai_strerror(errval));
}

void ai_perr(const char *const prefix,
             const struct addrinfo *const ai,
             const char *const port,
             const char *const msg) {
  warn("%s: %s %.*s on port %s: %s", prefix,
       addrfam(ai->ai_family),
       addrlen(ai->ai_family),
       addrip(ai->ai_family, ai->ai_addr),
       port, msg);
}

void ai_pmsg(const char *const prefix,
             const struct addrinfo *const ai,
             const char *const port) {
  warnx("%s: %s %.*s on port %s", prefix,
        addrfam(ai->ai_family),
        addrlen(ai->ai_family),
        addrip(ai->ai_family, ai->ai_addr),
        port);
}

void sa_pmsg(const char *const prefix,
             const int fam,
             const struct sockaddr *const sa) {
  warnx("%s: %s %.*s", prefix,
        addrfam(fam),
        addrlen(fam),
        addrip(fam, sa));
}
