#include <GL/glut.h>
#include <stdlib.h>
#include "map.h"
#include "global.h"

map *newMap ()
{
	map *new_map = calloc (1, sizeof (map));
	new_map->walls = NULL;
	new_map->walls_len = 0;
	return new_map;
}

void map_add_wall (map *m, wall *w)
{
	m->walls_len++;
	m->walls = realloc (m->walls, (m->walls_len) * sizeof (wall));
	m->walls[m->walls_len-1] = *w;
}

inline void renderMap (map *m)
{
	int i = 0;
	while (i < m->walls_len)
	{
		wall_draw (&m->walls[i]);
		i++;
	}
}

boolean map_can_be_here (map *m, int x, int z)
{
	int i = 0;
	boolean check = FALSE;
	while (i < m->walls_len)
	{
		check = wall_can_be_here (&m->walls[i], x, z);
		if (!check) return FALSE;
		i++;
	}
	return TRUE;
}
