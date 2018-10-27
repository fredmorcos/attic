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

#ifndef __MAP
#define __MAP

#include "wall.h"
#include "global.h"

#define MAP_WIDTH 20
#define MAP_HEIGHT 20
#define MAP_CELL_SIZE 0.3

struct _map
{
	struct _wall *walls;
	int walls_len;
};
typedef struct _map map;

map *newMap (void);
void map_add_wall (map *m, struct _wall *w);
void renderMap (map *m);
boolean map_can_be_here (map *m, int x, int z);

#endif
