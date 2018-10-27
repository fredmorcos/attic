#ifndef PET_MATHX_H
#define PET_MATHX_H

#define _unused     __attribute__((unused))
#define _malloc     __attribute__((malloc))
#define _deprecated __attribute__((deprecated))
#define _pure       __attribute__((pure))
#define _const      __attribute__((const))
#define _cleanup(x) __attribute__((cleanup(x)))

#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a > b ? b : a)

int iselem (int c, char const *l);

#endif
