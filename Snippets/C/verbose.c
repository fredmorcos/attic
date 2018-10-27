#include <stdarg.h>
#include <stdbool.h>
#include <bsd/bsd.h>
#include "verbose.h"

#define U __attribute__((unused))

static void verbosew_q(U const char *const fmt, ...) {}
static void verbosew_v(const char *const fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vwarn(fmt, ap);
  va_end(ap);
}

static void verbosewx_q(U const char *const fmt, ...) {}
static void verbosewx_v(const char *const fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vwarnx(fmt, ap);
  va_end(ap);
}

static void verbosewc_q(U const int code, U const char *const fmt, ...) {}
static void verbosewc_v(const int code, const char *const fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vwarnc(code, fmt, ap);
  va_end(ap);
}

void (*verbosew)(const char *const, ...) = verbosew_q;
void (*verbosewx)(const char *const, ...) = verbosewx_q;
void (*verbosewc)(const int, const char *const, ...) = verbosewc_q;

void verbose_set(void) {
  verbosew = verbosew_v;
  verbosewx = verbosewx_v;
  verbosewc = verbosewc_v;
}

#undef U
