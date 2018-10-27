#ifndef __GROUND
#define __GROUND

#include "texture.h"

struct _ground
{
	int iWidth, iHeight, iDepth;
	unsigned int uiTextureId;
};
typedef struct _ground ground;

ground *newGround (int iw, int ih, int id);
inline void renderGround (ground *g);

#endif
