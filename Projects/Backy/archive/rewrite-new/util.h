#pragma once

#include <stdbool.h>
#include <bsd/bsd.h>

double  timediff    (clock_t c);
char   *humansize   (double size);
bool    find_nchars (const char *const buf,
                     const size_t len,
                     const char ch,
                     const size_t num);

__attribute__((format (printf, 3, 4)))
int asprintf(size_t *const len,
             char **const str,
             const char *const fmt, ...);
