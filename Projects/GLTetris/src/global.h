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

#ifndef __GLOBAL
#define __GLOBAL

#define WIN_WID 500				/* window width */
#define WIN_HEI 600				/* window height */
#define GRID_STEP 30			/* cubed grid size */
#define GRID_COLS 10
#define GRID_ROWS 20

enum _boolean
{
	FALSE = 0,
	TRUE = 1
};

typedef enum _boolean boolean;

#endif
