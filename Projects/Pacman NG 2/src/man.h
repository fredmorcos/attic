#ifndef __MAN
#define __MAN

#include "global.h"

struct _man
{
	int ix, iy, iz;
	boolean bEvil;
};
typedef struct _man man;

man *newMan (float ix, float iy, float iz, boolean be);

inline void renderMan (man *m);

#endif
