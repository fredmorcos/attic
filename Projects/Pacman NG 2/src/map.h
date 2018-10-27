#ifndef __MAP
#define __MAP

#include "wall.h"
#include "global.h"

#define MAP_WIDTH 20
#define MAP_HEIGHT 20
#define MAP_CELL_SIZE 0.3

struct _map
{
	wall *walls;
	int walls_len;
};
typedef struct _map map;

map *newMap (void);
void map_add_wall (map *m, wall *w);
inline void renderMap (map *m);
boolean map_can_be_here (map *m, int x, int z);

#endif
