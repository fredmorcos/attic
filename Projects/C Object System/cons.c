#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "obj.h"
#include "cons.h"

extern const char *const __progname;

static void cons_destroy(struct cons *const cons);
static void cons_lock(struct cons *const cons);
static void cons_unlock(struct cons *const cons);

int cons_init(struct cons *const cons, const bool autofree) {
  obj_init(&cons->parent, autofree, (obj_dtor *) cons_destroy);
  return pthread_mutex_init(&cons->mutex, NULL);
}

static void cons_destroy(struct cons *const cons) {
  pthread_mutex_destroy(&cons->mutex);
}

static void cons_lock(struct cons *const cons) {
  int rc = pthread_mutex_lock(&cons->mutex);
  assert(rc == 0);
}

static void cons_unlock(struct cons *const cons) {
  int rc = pthread_mutex_unlock(&cons->mutex);
  assert(rc == 0);
}

__attribute__((format (printf, 2, 3)))
void cons_warn(struct cons *const cons,
               const char *const fmt, ...) {
  int tmp_errno = errno;

  cons_lock(cons);

  (void) fprintf(stderr, "%s: ", __progname);

  if (fmt) {
    va_list ap;

    va_start(ap, fmt);
    (void) vfprintf(stderr, fmt, ap);
    va_end(ap);

    (void) fprintf(stderr, ": ");
  }

  (void) fprintf(stderr, "%s\n", strerror(tmp_errno));

  cons_unlock(cons);
}

__attribute__((format (printf, 2, 3)))
void cons_warnx(struct cons *const cons,
                const char *const fmt, ...) {
  cons_lock(cons);

  (void) fprintf(stderr, "%s: ", __progname);

  if (fmt) {
    va_list ap;

    va_start(ap, fmt);
    (void) vfprintf(stderr, fmt, ap);
    va_end(ap);
  }

  (void) fprintf(stderr, "\n");

  cons_unlock(cons);
}

__attribute__((format (printf, 3, 4)))
void cons_warnc(struct cons *const cons,
                const int err_code,
                const char *const fmt, ...) {
  cons_lock(cons);

  (void) fprintf(stderr, "%s: ", __progname);

  if (fmt != NULL) {
    va_list ap;

    va_start(ap, fmt);
    (void) vfprintf(stderr, fmt, ap);
    va_end(ap);

    (void) fprintf(stderr, ": ");
  }

  (void) fprintf(stderr, "%s\n", strerror(err_code));

  cons_unlock(cons);
}
