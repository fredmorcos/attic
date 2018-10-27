#ifndef __EXTRA_H__
#define __EXTRA_H__

#include <stdarg.h>

#ifdef DEBUG
#include <stdio.h>
#define PRINT_INFO(s) printf("debug-info:%s:%d:" s "\n", __FILE__, __LINE__)
#define PRINT_INFO_LIT(s) PRINT_INFO(#s)
#else /* DEBUG */
#define PRINT_INFO(s) 0
#define PRINT_INFO_LIT(s) 0
#endif /* DEBUG */

void extra_die(const char *, ...);

#endif /* __EXTRA_H__ */
