#ifndef __VERBOSE_H__
#define __VERBOSE_H__

#include <stdio.h>

extern int global_verbose;

// #define msg(x) (global_verbose == 1 ? puts(x) : 0)
#define msg(x) (global_verbose ? printf(x "\n") : 0)

#endif /* __VERBOSE_H__ */

