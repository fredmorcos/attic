#ifndef PET_LOG
#define PET_LOG

#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <errno.h>

extern bool __debug;
extern bool __quiet;

#define __log_tag(tag) fprintf(stderr, "[" tag "] %s:%s:%d -> ",               \
                               __FILE__, __FUNCTION__, __LINE__)

#define __log_generic(tag, ...) {                                              \
    __log_tag(tag);                                                            \
    fprintf(stderr, __VA_ARGS__);                                              \
    fputs("\n", stderr);                                                       \
  }

#define log_status(...) {                                                      \
  if (__quiet != true) {                                                       \
    __log_generic("STATUS", __VA_ARGS__);                                      \
  }
#define log_error(...) __log_generic("ERROR ", __VA_ARGS__)
#define log_panic(...) __log_generic("PANIC ", __VA_ARGS__)
#define log_syserr(err) {                                                      \
    __log_tag("SYSERR");                                                       \
    errno = err;                                                               \
    perror(NULL);                                                              \
  }
#define log_debug(...)  {                                                      \
    if (__debug == true) {                                                     \
      __log_generic("DEBUG ", __VA_ARGS__);                                    \
    }                                                                          \
  }

#endif
