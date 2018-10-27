#pragma once

#include <stdbool.h>
#include <netdb.h>
#include "vec.h"

bool netsend(const int fd, const char *const buf, const size_t len)
  __attribute__((warn_unused_result, nonnull));

int netrecv(const int fd, const size_t chunk, struct vec *const buf)
  __attribute__((warn_unused_result, nonnull));

int xbindlisten(const struct addrinfo *const ai)
  __attribute__((warn_unused_result, nonnull));

int xconnect(const struct addrinfo *const ai)
  __attribute__((warn_unused_result, nonnull));

void sock_close(const int *const fd)
  __attribute__((nonnull));
