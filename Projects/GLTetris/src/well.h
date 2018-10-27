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

#ifndef __WELL
#define __WELL

#include "cube.h"
#include "global.h"
#include "piece.h"

struct _well
{
	int removed_lines;
	cube *list [GRID_COLS][GRID_ROWS];
};

typedef struct _well well;

well *well_new ();
void well_unload (well *w);
void well_draw (well *w);
boolean well_add_piece (well *w, piece *p, void (*gameoverfunc) (void));
boolean well_check_move_right (well *w, piece *p);
boolean well_check_move_left (well *w, piece *p);
boolean well_check_move_down (well *w, piece *p);
void well_remove_lines (well *w);
#endif
