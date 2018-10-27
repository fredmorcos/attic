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

#ifndef __GRID
#define __GRID

#define G_R 150.0/255.0
#define G_G 170.0/255.0
#define G_B 180.0/255.0

/* a grid is the object that should the 
 * horizontal and vertical lines in a 
 * tetris game. just for the looks.
 */
struct _grid
{
	int step, width, height;
	float r, g, b;
};

typedef struct _grid grid;

grid *grid_new ();
void grid_draw (grid *g);

#endif
