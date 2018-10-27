#pragma once

#include "obj.h"
#include "str.h"

struct sock {
  struct obj parent;
  struct str description;
  int fd;
};

int sock_init(struct sock *const sock,
              const bool autofree,
              const char *const description);
