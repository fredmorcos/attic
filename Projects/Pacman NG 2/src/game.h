#ifndef __GAME
#define __GAME

#include "map.h"
#include "bille.h"
#include "man.h"

void initMap (map *pMap);
inline void renderBilles (bille *pBilles [85]);
void initBilles (map *pMap, bille *pBilles [85], man *m);

#endif
