#include <assert.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include "ai.h"

/**
 * Special handling for EAI_AGAIN and EAI_SYSTEM.
 */
int xgetai(const char *const host,
           const char *const port,
           const int family,
           const int socktype,
           const int flags,
           AddrInfo **result) {
  assert(port != NULL);
  assert(result != NULL);

  const struct addrinfo hints = {
    .ai_family   = family,
    .ai_socktype = socktype,
    .ai_flags    = flags
  };

  int ret = getaddrinfo(host, port, &hints, result);

  assert(ret != EAI_BADFLAGS);
  assert(ret != EAI_SOCKTYPE);

  return (ret);
}

void ai_free(AddrInfo **const ai) {
  assert(ai != NULL);

  if (*ai != NULL)
    freeaddrinfo(*ai);
}

void ai_print(const char *const prefix,
              const int family,
              SockAddr *const addr,
              const char *const port,
              const bool handle_errno) {
  assert(prefix != NULL);
  assert(addr != NULL);
  assert(port != NULL);

  void (*warn_func)(const char *format, ...)
    attr((format(printf, 1, 2)))
    = handle_errno ? warn : warnx;

  int _errno = errno;

  if (family == AF_INET) {
    char ip[INET_ADDRSTRLEN];
    struct sockaddr_in sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in));

    if (inet_ntop(AF_INET, &sa.sin_addr, ip, INET_ADDRSTRLEN) == NULL) {
      assert(errno != EAFNOSUPPORT);
      assert(errno != ENOSPC);
    }

    errno = _errno;
    warn_func("%s IPv4:%.*s:%s", prefix, INET_ADDRSTRLEN, ip, port);
  } else if (family == AF_INET6) {
    char ip[INET6_ADDRSTRLEN];
    struct sockaddr_in6 sa;

    (void) memcpy(&sa, addr, sizeof(struct sockaddr_in6));

    if (inet_ntop(AF_INET6, &sa.sin6_addr, ip, INET6_ADDRSTRLEN) == NULL) {
      assert(errno != EAFNOSUPPORT);
      assert(errno != ENOSPC);
    }

    errno = _errno;
    warn_func("%s IPv6:%.*s:%s", prefix, INET6_ADDRSTRLEN, ip, port);
  } else {
    assert(family == AF_INET || family == AF_INET6);
  }

  /* just  in case  this function  implementation changes,  we restore
   * errno here
   */
  errno = _errno;
}
