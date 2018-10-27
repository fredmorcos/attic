#include <errno.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include "obj.h"
#include "str.h"
#include "sock.h"

static void sock_destroy(struct sock *sock);

int sock_init(struct sock *const sock,
              const bool autofree,
              const char *const description) {
  int rc = 0;

  obj_init(&sock->parent, autofree, (obj_dtor *) sock_destroy);

  rc = str_init(&sock->description, true, description);

  if (rc != 0) {
    obj_unref((struct obj *const) &sock->description);
    return rc;
  }

  return 0;
}

int sock_connect(const char *const host, const char *const port) {
  int gai_rc = 0;
  int opt = 1;

  struct addrinfo hints;
  struct addrinfo *res = NULL;

  (void) memset(&hints, 0, sizeof(struct addrinfo));
}

static void sock_destroy(struct sock *const sock) {
  close(sock->fd);
  obj_unref((struct obj *const) &sock->description);
}
