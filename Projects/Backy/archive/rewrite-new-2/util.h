#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <dirent.h>
#include "vec.h"

#define clean(x) __attribute__((cleanup(x)))

typedef long double ldouble;
typedef long long ll;
typedef unsigned long long ull;
typedef unsigned int uint;
typedef unsigned char uchar;

static const char NIL = '\0';

struct time_info {
  clock_t begin_cpu;
  clock_t end_cpu;

  struct timespec begin_rt;
  struct timespec end_rt;

  struct timeval begin_wc;
  struct timeval end_wc;

  ldouble cpu;
  ldouble wc;
  ldouble rt;
};

typedef void *(pt_func)(void *);

void iopbufstat(const size_t nent,
                const size_t size,
                const struct time_info *const ti);

bool xchdir(DIR *const d, const char **const err_msg);
void xcldir(DIR **d);

void ptcancel_en(void);
void ptcancel_dis(void);
void ptsetcancel(void);
void ptdestroy(pthread_t *t);

uint ndigits(ull val) __attribute__((const));

void ulltoa(char *str, ull val, const uint len);

char *xrealpath(const char *const p) __attribute__((malloc));

void tinow(struct time_info *const ti, const bool begin);
void tidiff(struct time_info *const ti);

char *hsize(ldouble size) __attribute__((malloc));

int xasprintf(size_t *const l, char **const s, const char *const f, ...)
  __attribute__((format (printf, 3, 4)));

void *xrealloc(void *optr, const size_t nmemb, const size_t size)
  __attribute__((malloc));

size_t strlcpy(char *dst, const char *src, const size_t dsize);
