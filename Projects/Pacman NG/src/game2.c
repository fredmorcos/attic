/*
 * This file is part of pacman-ng
 *
 * Hex packet sniffer
 * Copyright (C) 2007  Frederic-Gerald Morcos
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

#include "game.h"

#include <stdlib.h>
#include "map.h"
#include "wall.h"
#include "bille.h"
#include "man.h"

void initBilles (map *pMap, bille *pBilles [85], man *m)
{
	int z = -5, i = 0, x = -5;
	while (x <= 5)
	{
		z = -5;
		while (z <= 5)
		{
			if (m->ix != x || m->iz != z)
				if (map_can_be_here (pMap, x, z))
				{
					pBilles [i] = newBille (x, 0, z, TRUE);
					i++;
				}
				z++;
		}
		x++;
	}
}

inline void renderBilles (bille *pBilles [85])
{
	int i = 0;
	while (i < 85)
	{
		if (pBilles [i] != NULL)
			renderBille (pBilles [i]);
		i++;
	}
}

void initMap (map *pMap)
{
	wall *w1 = wall_new (5.0, 0.0, 5.0, HORIZONTAL, 1);
	map_add_wall (pMap, w1);

	wall *w2 = wall_new (2.0, 0.0, 5.0, HORIZONTAL, 1);
	map_add_wall (pMap, w2);

	wall *w3 = wall_new (-2.0, 0.0, 4.0, VERTICAL, 3);
	map_add_wall (pMap, w3);

	wall *w4 = wall_new (-4.5, 0.0, 5.0, HORIZONTAL, 2);
	map_add_wall (pMap, w4);

	wall *w5 = wall_new (3.0, 0.0, 4.0, HORIZONTAL, 1);
	map_add_wall (pMap, w5);

	wall *w6 = wall_new (0.0, 0.0, 2.0, VERTICAL, 5);
	map_add_wall (pMap, w6);

	wall *w7 = wall_new (4.0, 0.0, 3.0, HORIZONTAL, 1);
	map_add_wall (pMap, w7);

	wall *w8 = wall_new (2.0, 0.0, 3.0, HORIZONTAL, 1);
	map_add_wall (pMap, w8);

	wall *w9 = wall_new (-4.5, 0.0, 3.0, HORIZONTAL, 2);
	map_add_wall (pMap, w9);

	wall *w10 = wall_new (-5.0, 0.0, 2.0, HORIZONTAL, 1);
	map_add_wall (pMap, w10);

	wall *w11 = wall_new (4.5, 0.0, 1.0, HORIZONTAL, 2);
	map_add_wall (pMap, w11);

	wall *w12 = wall_new (-0.5, 0.0, 1.0, HORIZONTAL, 6);
	map_add_wall (pMap, w12);

	wall *w13 = wall_new (-4.0, 0.0, -0.5, VERTICAL, 2);
	map_add_wall (pMap, w13);

	wall *w14 = wall_new (5.0, 0.0, -1.5, VERTICAL, 2);
	map_add_wall (pMap, w14);

	wall *w15 = wall_new (2.5, 0.0, -1.0, HORIZONTAL, 2);
	map_add_wall (pMap, w15);

	wall *w16 = wall_new (-1.5, 0.0, -1.0, HORIZONTAL, 2);
	map_add_wall (pMap, w16);

	wall *w17 = wall_new (1.0, 0.0, -2.0, VERTICAL, 1);
	map_add_wall (pMap, w17);

	wall *w18 = wall_new (-1.0, 0.0, -2.0, VERTICAL, 1);
	map_add_wall (pMap, w18);

	wall *w19 = wall_new (2.0, 0.0, -3.0, HORIZONTAL, 3);
	map_add_wall (pMap, w19);

	wall *w20 = wall_new (-3.0, 0.0, -3.5, VERTICAL, 2);
	map_add_wall (pMap, w20);

	wall *w21 = wall_new (-5.0, 0.0, -3.0, VERTICAL, 1);
	map_add_wall (pMap, w21);

	wall *w22 = wall_new (3.0, 0.0, -4.0, HORIZONTAL, 3);
	map_add_wall (pMap, w22);

	wall *w23 = wall_new (-1.0, 0.0, -4.5, VERTICAL, 2);
	map_add_wall (pMap, w23);

	wall *w24 = wall_new (0.0, 0.0, -5.0, HORIZONTAL, 1);
	map_add_wall (pMap, w24);

	wall *w25 = wall_new (-4.0, 0.0, -5.0, HORIZONTAL, 1);
	map_add_wall (pMap, w25);

	wall *w26 = wall_new (6.0, 0.0, 0.0, VERTICAL, 13);
	map_add_wall (pMap, w26);

	wall *w27 = wall_new (0.0, 0.0, -6.0, HORIZONTAL, 13);
	map_add_wall (pMap, w27);

	wall *w28 = wall_new (-6.0, 0.0, 0.0, VERTICAL, 13);
	map_add_wall (pMap, w28);

	wall *w29 = wall_new (0.0, 0.0, 6.0, HORIZONTAL, 13);
	map_add_wall (pMap, w29);
}
