#pragma once
#define attr __attribute__

#include <stdbool.h>
#include <netdb.h>

typedef struct sockaddr SockAddr;
typedef struct addrinfo AddrInfo;

#define AutoAddrInfo attr((cleanup(ai_free))) AddrInfo

void ai_free(AddrInfo **const ai)
  attr((nonnull));

void ai_print(const char *const prefix,
              const int family,
              SockAddr *const addr,
              const char *const port,
              const bool handle_errno)
  attr((nonnull));

int xgetai(const char *const host,
           const char *const port,
           const int family,
           const int socktype,
           const int flags,
           AddrInfo **result)
  attr((warn_unused_result, nonnull(2, 6)));
