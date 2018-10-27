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

#ifndef __CUBE
#define __CUBE

#include "global.h"

/* cube object, with position and color */
struct _cube
{
	/* when used in piece, x and y are relative to the piece's x and y,
	 * when used in the well, these are absolute value relative to the grid
	 */
	int x, y;
	float r, g, b;
};

typedef struct _cube cube;

inline cube *cube_new (void);
inline void cube_set_position (cube *c, int nx, int ny);
inline void cube_draw (cube *c, int x, int y);
inline void cube_set_color (cube *c, float nr, float ng, float nb);

#endif
