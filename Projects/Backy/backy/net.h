#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <netdb.h>
#include "ai.h"
#include "vec.h"

bool netsend(const int fd, const char *const buf, const size_t len)
  attr((warn_unused_result, nonnull));

int netrecv(const int fd, const size_t chunk, Vec *const buf)
  attr((warn_unused_result, nonnull));

int xbindlisten(const AddrInfo *const ai)
  attr((warn_unused_result, nonnull));

int xconnect(const AddrInfo *const ai)
  attr((warn_unused_result, nonnull));

#define AutoSock \
  attr((cleanup(sock_close))) int

void sock_close(const int *const fd)
  attr((nonnull));
