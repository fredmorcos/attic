/*
 * This file is part of pacman-ng
 * 
 * Pacman Next Generation
 * Copyright (C) 2007  Frederic-Gerald Morcos
 * 									   Marleine Daoud
 * 									   Andrew Botros
 * 
 * pacman-ng is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * pacman-ng is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with pacman-ng.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <GL/glut.h>
#include <stdlib.h>
#include "map.h"
#include "global.h"
#include "wall.h"

map *newMap ()
{
	map *new_map = calloc (1, sizeof (map));
	new_map->walls = NULL;
	new_map->walls_len = 0;
	return new_map;
}

void map_add_wall (map *m, struct _wall *w)
{
	m->walls_len++;
	m->walls = realloc (m->walls, (m->walls_len) * sizeof (struct _wall));
	m->walls[m->walls_len - 1] = *w;
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

