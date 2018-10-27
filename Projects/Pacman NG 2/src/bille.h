#ifndef __BILLE
#define __BILLE

#include "global.h"

struct _bille
{
	int ix, iy, iz;
	boolean bVisible;
};

typedef struct _bille bille;

bille *newBille (int ix, int iy, int iz, boolean bv);
inline void renderBille (bille *b);
boolean set_bille_visibility (bille *pBilles [85], int x, int z, int *iScore);

#endif

