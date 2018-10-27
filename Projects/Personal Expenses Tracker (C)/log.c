#include "log.h"
#include <stdio.h>
#include <string.h>

void log_err (const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  (void) vfprintf(stderr, fmt, ap);
  va_end(ap);

  (void) fprintf(stderr, "\n");
}

void log_verr (const char *fmt, va_list ap) {
  (void) vfprintf(stderr, fmt, ap);
  (void) fprintf(stderr, "\n");
}

void log_syserr (const int errnum, const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  (void) vfprintf(stderr, fmt, ap);
  va_end(ap);

  (void) fprintf(stderr, ": %s\n", strerror(errnum));
}
