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

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "piece.h"
#include "global.h"

void piece_set_color (piece *p);
void piece_reload (piece *p);

/* return a random piece type */
inline piece_type get_random_type ()
{
	return abs ((((time (NULL)) % 60) * rand ()) % 7);
}

/* create a new piece */
piece *piece_new (int nx, int ny, piece_type nt)
{
	piece *newp = malloc (sizeof (piece));
	
	int i = 0;
	while (i < 4)
	{
		newp->list [i] = cube_new ();
		i++;
	}
	
	newp->rotation = UP;					/* default rotation */
	piece_set_type (newp, nt);
	piece_set_position (newp, nx, ny);
	piece_reload (newp);
	
	return newp;
}

/*
 * Cube 2 is the rightmost cube, we use to decide if we 
 * should go one more to the right or not
 */
void piece_move_right (piece *p)
{	
	if (!((p->x) + ((p->list [2])->x) >= GRID_COLS - 1) && p != NULL)
		(p->x)++;
}

/*
 * Cube 1 is the leftmost cube, we use to decide if we 
 * should go one more to the left or not
 */
void piece_move_left (piece *p)
{	
	if (!((p->x) + ((p->list [1])->x) <= 0) && p != NULL)
		(p->x)--;
}

/*
 * Cube 3 is the bottom cube, we use to decide if we 
 * should go one more downwards or not
 */
boolean piece_move_down (piece *p)
{	
	if (!((p->y) + ((p->list [3])->y) >= GRID_ROWS - 1) && p != NULL)
		(p->y)++;
	else
		return FALSE;
	
	return TRUE;
}

/* set the type of a piece */
inline void piece_set_type (piece *p, piece_type nt)
{
	if (p == NULL) return;
	
	p->type = nt;
	piece_reload (p);
}

/* will set the color for a piece depending on it's type */
void piece_set_color (piece *p)
{
	if (p == NULL) return;

	int i = 0;
	while (i < 4)
	{
		switch (p->type)
		{
			case I:	cube_set_color (p->list [i], I_R, I_G, I_B); break;
			case T:	cube_set_color (p->list [i], T_R, T_G, T_B); break;
			case L:	cube_set_color (p->list [i], L_R, L_G, L_B); break;
			case J:	cube_set_color (p->list [i], J_R, J_G, J_B); break;
			case S:	cube_set_color (p->list [i], S_R, S_G, S_B); break;
			case Z:	cube_set_color (p->list [i], Z_R, Z_G, Z_B); break;
			case Q:	cube_set_color (p->list [i], Q_R, Q_G, Q_B); break;
			default: printf ("ERROR: piece_set_color: Unknown type.\n");
		}
		i++;
	}
}

/* draw a piece, use this from your display callback */
void piece_draw (piece *p)
{
	if (p == NULL) return;
	
	int i = 0;
	while (i < 4)
	{
		cube_draw (p->list[i], (p->x) + (p->list[i])->x, (p->y) + (p->list[i])->y);
		i++;
	}
}

/* free the piece's allocated memory, used when exiting */
void piece_unload (piece *p)
{
	if (p == NULL) return;
	
	int i = 0;
	while (i < 4)
	{
		free (p->list[i]);
		i++;
	}

	free (p);
}

/*
 * Use this function carefully, there is no 
 * boundary checking here!
 */
void piece_set_position (piece *p, int nx, int ny)
{
	if (p == NULL) return;
	
	p->x = nx;
	p->y = ny;
}

/* change the rotation information of a piece */
void piece_rotate (piece *p)
{
	if (p->y < 0 || p == NULL) return;
	
	switch (p->rotation)
	{
		case UP:	p->rotation = RIGHT;	break;
		case RIGHT:	p->rotation = DOWN;		break;
		case DOWN:	p->rotation = LEFT;		break;
		case LEFT:	p->rotation = UP;		break;
		default:	printf ("ERROR: piece_rotate: Unknown rotation.\n");
	}
	
	piece_reload (p);
}

/* 
 * cube 0 = UP
 * cube 1 = LEFT
 * cube 2 = RIGHT
 * cube 3 = DOWN
 * 
 * NO MATTER WHAT ROTATION OR WHAT TYPE, BOUNDARIES 
 * WILL BE CHECKED USING THIS CONVENTION!!! SO FOLLOW IT!!!
 */
void piece_reload (piece *p)
{
	piece_set_color (p);
	if (p->type == I)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 0, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 0, 2);
			cube_set_position (p->list [3], 0, 3);
		}
		else if (p->rotation == RIGHT)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 0);
			cube_set_position (p->list [2], 3, 0);
			cube_set_position (p->list [3], 2, 0);
		}
		else if (p->rotation == DOWN)
		{
			cube_set_position (p->list [0], 0, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 0, 2);
			cube_set_position (p->list [3], 0, 3);
		}
		else if (p->rotation == LEFT)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 0);
			cube_set_position (p->list [2], 3, 0);
			cube_set_position (p->list [3], 2, 0);
		}
	}
	else if (p->type == T)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 2, 1);
			cube_set_position (p->list [3], 1, 1);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
	else if (p->type == L)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 0, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 1, 2);
			cube_set_position (p->list [3], 0, 2);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
	else if (p->type == J)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 2);
			cube_set_position (p->list [2], 1, 1);
			cube_set_position (p->list [3], 1, 2);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
	else if (p->type == S)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 2, 0);
			cube_set_position (p->list [3], 1, 1);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
	else if (p->type == Z)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 1, 0);
			cube_set_position (p->list [1], 0, 0);
			cube_set_position (p->list [2], 2, 1);
			cube_set_position (p->list [3], 1, 1);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
	else if (p->type == Q)
	{
		if (p->rotation == UP)
		{
			cube_set_position (p->list [0], 0, 0);
			cube_set_position (p->list [1], 0, 1);
			cube_set_position (p->list [2], 1, 0);
			cube_set_position (p->list [3], 1, 1);
		}
		else if (p->rotation == RIGHT)
		{
		}
		else if (p->rotation == DOWN)
		{
		}
		else if (p->rotation == LEFT)
		{
		}
	}
}
