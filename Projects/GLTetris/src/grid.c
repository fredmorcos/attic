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

#include <stdlib.h>
#include <GL/glut.h>

#include "grid.h"
#include "global.h"

/* create a new grid */
grid *grid_new ()
{
	grid *newg = malloc (sizeof (grid));
	newg->step = GRID_STEP;
	newg->width = GRID_COLS;
	newg->height = GRID_ROWS;
	newg->r = G_R;
	newg->g = G_G;
	newg->b = G_B;
	
	return newg;
}

/* draw the grid, should be called from the 
 * display callback function
 */
void grid_draw (grid *g)
{
	int i = 0;
	
	glColor3f (g->r, g->g, g->b);
	
	glBegin (GL_LINES);
	while (i <= GRID_COLS * GRID_STEP)
	{
		glVertex2i (i, 0);
		glVertex2i (i, GRID_ROWS * GRID_STEP);
		
		i += GRID_STEP;
	}
	
	i = 0;
	
	while (i <= GRID_ROWS * GRID_STEP)
	{
		glVertex2i (0, i);
		glVertex2i (GRID_COLS * GRID_STEP, i);
		i += GRID_STEP;
	}
	glEnd ();
}
