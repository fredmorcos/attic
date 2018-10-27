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

#include "well.h"
#include "global.h"
#include "piece.h"
#include "cube.h"

#include <stdio.h>
#include <stdlib.h>
#include <GL/glut.h>

inline void remove_line (well *w, int line);
boolean check_game_over (well *w);
void down_lines (well *w, int line);
inline void copy_row_down (well *w, int line);

/* create a new well */
well *well_new ()
{
	well *neww = malloc (sizeof (well));
	neww->removed_lines = 0;
	return neww;
}

/* check if a piece can go left */
boolean well_check_move_left (well *w, piece *p)
{
	int i = 0, x = 0, y = 0;
	while (i < 4)
	{
		x = p->x + (p->list [i])->x;
		y = p->y + (p->list [i])->y;
		
		if (w->list [x - 1][y] != NULL)
			return FALSE;
		
		i++;
	}
	
	return TRUE;
}

/* check if a piece can go right */
boolean well_check_move_right (well *w, piece *p)
{
	int i = 0, x = 0, y = 0;
	while (i < 4)
	{
		x = p->x + (p->list [i])->x;
		y = p->y + (p->list [i])->y;
		
		if (w->list [x + 1][y] != NULL)
			return FALSE;
		
		i++;
	}
	
	return TRUE;
}

/* check if a piece can go down */
boolean well_check_move_down (well *w, piece *p)
{
	int i = 0, x = 0, y = 0;
	while (i < 4)
	{
		x = p->x + (p->list [i])->x;
		y = p->y + (p->list [i])->y;
		
		if (w->list [x][y + 1] != NULL || y == GRID_ROWS - 1)
			return FALSE;
		
		i++;
	}
	
	return TRUE;
}

/* remove lines that are full :) */
void well_remove_lines (well *w)
{
	int i = GRID_ROWS - 1, j = 0, k = 0;
	boolean rem = TRUE, isnull = FALSE;

	while (k < GRID_ROWS)
	{
		while (i >= 0)
		{
			j = 0;
			rem = TRUE;
			while (j < GRID_COLS)
			{
				if (w->list [j][i] == NULL)
				{
					rem = FALSE;
					break;
				}
				j++;
			}
			
			if (rem == TRUE)
			{
				remove_line (w, i);
				w->removed_lines++;
			}
			
			i--;
		}
		k++;
	}
}

/* remove a single line */
inline void remove_line (well *w, int line)
{
	int j = 0;
	while (j < GRID_COLS)
	{
		w->list [j][line] = NULL;
		j++;
	}
	down_lines (w, line);
}

/* lower the lines above one that has been removed */
void down_lines (well *w, int line)
{
	int j = 0;
	while (line > 0)
	{
		copy_row_down (w, line);
		line--;
	}
}

/* copy a single row down and empty it */
inline void copy_row_down (well *w, int line)
{
	if (line < 0)
		return;
	
	int j = 0;
	while (j < GRID_COLS)
	{
		w->list [j][line] = w->list [j][line - 1];
		w->list [j][line - 1] = NULL;
		j++;
	}
}

/* check if a piece can be added to the well, if so then add it */
boolean well_add_piece (well *w, piece *p, void (*gameoverfunc)(void))
{
	int i = 0, x = 0, y = 0;
	boolean add = FALSE;
	
	while (i < 4)
	{
		x = p->x + (p->list [i])->x;
		y = p->y + (p->list [i])->y;
		
		if (well_check_move_down (w, p) == FALSE)
		{
			add = TRUE;
			break;
		}
		i++;
	}
	
	if (add == TRUE)
	{
		i = 0;
		while (i < 4)
		{
			x = p->x + (p->list [i])->x;
			y = p->y + (p->list [i])->y;
			
			w->list [x][y] = cube_new ();
			(w->list [x][y])->r = (p->list [i])->r;
			(w->list [x][y])->g = (p->list [i])->g;
			(w->list [x][y])->b = (p->list [i])->b;
			
			i++;
		}
		
		int k = 0;
		while (k < GRID_ROWS)
		{
			well_remove_lines (w);
			glutPostRedisplay ();
			k++;
		}
		
		if (check_game_over (w) == TRUE)
			gameoverfunc ();
		return TRUE;
	}
	
	int k = 0;
	while (k < GRID_ROWS)
	{
		well_remove_lines (w);
		glutPostRedisplay ();
		k++;
	}
	
	if (check_game_over (w) == TRUE)
		gameoverfunc ();
	return FALSE;
}

/* check if the game is over */
boolean check_game_over (well *w)
{
	int i = 0;
	while (i < GRID_COLS)
	{
		if (w->list [i][0] != NULL)
			return TRUE;
			
		i++;
	}
	return FALSE;
}

/* free allocated memory for the well */
void well_unload (well *w)
{
	int i = 0, j = 0;
	while (i < GRID_COLS)
	{	
		j = 0;
		while (j < GRID_ROWS)
		{
			free (w->list [i][j]);
			j++;
		}
		i++;
	}
}

/* draw the well with all the cubes in it, call this from 
 * the display callback function 
 */
void well_draw (well *w)
{
	int i = 0, j = 0;
	while (i < GRID_COLS)
	{	
		j = 0;
		while (j < GRID_ROWS)
		{
			cube_draw (w->list [i][j], i, j);
			j++;
		}
		i++;
	}
}
