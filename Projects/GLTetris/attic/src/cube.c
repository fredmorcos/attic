/*
 * This file is part of GLTetris
 * 
 * GLTetris GLUT/OpenGL based Tetris.
 * Copyright (C) 2008  Frederic-Gerald Morcos
 * 
 * GLTetris is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * GLTetris is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GLTetris.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "cube.h"
#include "global.h"

#include <GL/glut.h>
#include <stdlib.h>

/* create a new cube */
inline cube *cube_new ()
{
	cube *newc = malloc (sizeof (cube));
	return newc;
}

/* set the rgb colors of the cube */
inline void cube_set_color (cube *c, float nr, float ng, float nb)
{
	if (c == NULL) return;
	
	c->r = nr;
	c->g = ng;
	c->b = nb;
}

/* set a cube's position */
inline void cube_set_position (cube *c, int nx, int ny)
{
	if (c == NULL) return;
	
	c->x = nx;
	c->y = ny;
}

/* draw the cube with it's color, should be called from
 * your display callback function
 */
inline void cube_draw (cube *c, int x, int y)
{	
	if (c == NULL) return;
	
	/* so we have the cube's drawing position according
	 * to the grid of the well...
	 */
	x *= GRID_STEP;
	y *= GRID_STEP;
	
	glColor3f (c->r, c->g, c->b);
	glRecti (x + 2, y + 1, x + GRID_STEP - 1, y + GRID_STEP - 2);
}

