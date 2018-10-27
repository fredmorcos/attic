#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/tcp.h>
#include "net.h"
#include "vec.h"
#include "mem.h"
#include "str.h"

#if !defined(NDEBUG)
#include <err.h>
#endif

bool netsend(const int fd, const char *const buf, const size_t len) {
  assert(fd >= 0);
  assert(buf != NULL);
  assert(len > 0);

  ssize_t send_rc = 0;
  size_t send_len = 0;

#if !defined(NDEBUG)
  {
    __attribute__((cleanup(char_free))) char *size = humansize(len);
    warnx("DEBUG netsend(): fd %d, %s (%zd bytes)", fd, size, len);
  }
#endif

  if ((send_rc = send(fd, buf, len, 0)) == -1) {
    assert(errno != EBADF);
    assert(errno != EDESTADDRREQ);
    assert(errno != ENOTCONN);
    assert(errno != ENOTSOCK);
    assert(errno != EOPNOTSUPP);
    return (false);
  } else if ((send_len = (size_t) send_rc) < len) {
    return (netsend(fd, buf + send_len, len - send_len));
  }

  assert(send_len == len);
  return (true);
}

int netrecv(const int fd, const size_t chunk, struct vec *const buf) {
  assert(fd >= 0);
  assert(chunk > 0);
  assert(buf != NULL);

  char *resp = NULL;
  ssize_t recvlen = 0;

  if (!(resp = vec_add(buf, chunk)))
    return (-1);

  if ((recvlen = recv(fd, resp, chunk, 0)) == 0) {
    return (-2);
  } else if (recvlen == -1) {
    assert(errno != EBADF);
    assert(errno != ENOTCONN);
    assert(errno != ENOTSOCK);
    return (-1);
  }

  assert((size_t) recvlen <= chunk);
  vec_rev(buf, chunk - (size_t) recvlen);

#if !defined(NDEBUG)
  {
    __attribute__((cleanup(char_free))) char *size = humansize(recvlen);
    __attribute__((cleanup(char_free))) char *size_tot = humansize(buf->len);
    warnx("DEBUG netrecv(): fd %d, received %s (%zd b), total %s (%zd b)",
          fd, size, recvlen, size_tot, buf->len);
  }
#endif

  if (strchars(resp, (size_t) recvlen, '\0', 2))
    return (0);
  return (netrecv(fd, chunk, buf));
}

int xbindlisten(const struct addrinfo *const ai) {
  assert(ai != NULL);

  const int opt = 1;               /* listening socket option value */
  int fd = -1;                     /* listening socket fd           */

  if ((fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol)) == -1)
    return (-1);

  (void) setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(int));
  (void) setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(int));
  (void) setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int));

  if (bind(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
    assert(errno != EBADF);
    assert(errno != ENOTSOCK);
    assert(errno != EINVAL);
    assert(errno != EISCONN);
    goto fail;
  }

  if (listen(fd, 0) == -1) {
    assert(errno != EBADF);
    assert(errno != EDESTADDRREQ);
    assert(errno != EINVAL);
    assert(errno != ENOTSOCK);
    goto fail;
  }

  return (fd);

 fail: {
    int _errno = errno;
    (void) close(fd);
    errno = _errno;
    return (-1);
  }
}

int xconnect(const struct addrinfo *const ai) {
  assert(ai != NULL);

  const int opt = 1;                  /* client socket option value */
  int fd = -1;                        /* client socket fd           */

  if ((fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol)) == -1)
    return (-1);

  (void) setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(int));

  if (connect(fd, ai->ai_addr, ai->ai_addrlen) == -1) {
    assert(errno != EALREADY);
    assert(errno != EBADF);
    assert(errno != EISCONN);
    assert(errno != ENOTSOCK);
    assert(errno != EINVAL);
    assert(errno != EOPNOTSUPP);
    (void) close(fd);
    return (-1);
  }

  return (fd);
}

void sock_close(const int *const fd) {
  assert(fd != NULL);

  int _errno = errno;
  if (*fd > -1)
    (void) close(*fd);
  errno = _errno;
}
