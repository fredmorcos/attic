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
    AutoStr size = humansize(len);
    warnx("DEBUG netsend(): %s (%zd bytes)", size, len);
  }
#endif

  send_rc = send(fd, buf, len, 0);

  if (send_rc == -1) {
    assert(errno != EBADF);
    assert(errno != EDESTADDRREQ);
    assert(errno != ENOTCONN);
    assert(errno != ENOTSOCK);
    assert(errno != EOPNOTSUPP);
    return (false);
  }

  send_len = (size_t) send_rc;

  if (send_len < len)
    return (netsend(fd, buf + send_len, len - send_len));

  assert(send_len == len);
  return (true);
}

int netrecv(const int fd, const size_t chunk, Vec *const buf) {
  assert(fd >= 0);
  assert(chunk > 0);
  assert(buf != NULL);

  char *resp = NULL;
  ssize_t recvlen = 0;

  resp = vec_add(buf, chunk);

  if (resp == NULL)
    return (-1);

  recvlen = recv(fd, resp, chunk, 0);

  if (recvlen == 0)
    return (-2);

  if (recvlen == -1) {
    assert(errno != EBADF);
    assert(errno != ENOTCONN);
    assert(errno != ENOTSOCK);
    return (-1);
  }

  assert((size_t) recvlen <= chunk);
  vec_rev(buf, chunk - (size_t) recvlen);

#if !defined(NDEBUG)
  {
    AutoStr size = humansize(recvlen);
    AutoStr size_tot = humansize(buf->len);
    warnx("DEBUG netrecv(): r %s (%zd b), t %s (%zd b)",
          size, recvlen, size_tot, buf->len);
  }
#endif

  if (strchars(resp, (size_t) recvlen, '\0', 2))
    return (0);

  return (netrecv(fd, chunk, buf));
}

int xbindlisten(const AddrInfo *const ai) {
  assert(ai != NULL);

  /* listening socket option value */
  const int opt = 1;

  /* listening socket fd */
  int fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);

  if (fd == -1)
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

int xconnect(const AddrInfo *const ai) {
  assert(ai != NULL);

  /* client socket option value */
  const int opt = 1;

  /* client socket fd */
  int fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);

  if (fd == -1)
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
