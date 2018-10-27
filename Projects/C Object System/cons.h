#pragma once

#include <pthread.h>
#include <stdbool.h>
#include "obj.h"

struct cons {
  struct obj parent;
  pthread_mutex_t mutex;
};

int cons_init(struct cons *const cons, const bool autofree);

__attribute__((format (printf, 2, 3)))
void cons_warn(struct cons *const cons,
               const char *const fmt, ...);

__attribute__((format (printf, 2, 3)))
void cons_warnx(struct cons *const cons,
                const char *const fmt, ...);

__attribute__((format (printf, 3, 4)))
void cons_warnc(struct cons *const cons,
                const int err_code,
                const char *const fmt, ...);
