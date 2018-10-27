#ifndef PET_LOG_H
#define PET_LOG_H

#include <stdarg.h>

void log_err (const char *fmt, ...);
void log_verr (const char *fmt, va_list ap);

void log_syserr (int errnum, const char *fmt, ...);

#endif
