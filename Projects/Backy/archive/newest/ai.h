#pragma once

#include <stdbool.h>
#include <netdb.h>

void ai_free(struct addrinfo **const ai)
  __attribute__((nonnull));

void ai_print(const char *const prefix,
              const int family,
              struct sockaddr *const addr,
              const char *const port,
              const bool handle_errno)
  __attribute__((nonnull));

int xgetai(const char *const host,
           const char *const port,
           const int family,
           const int socktype,
           const int flags,
           struct addrinfo **result)
  __attribute__((warn_unused_result, nonnull(2, 6)));
