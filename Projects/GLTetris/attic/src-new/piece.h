/*
 * This file is part of GLTetris
 * 
 * GLTetris GLUT/OpenGL based Tetris.
 * Copyright (C) 2008  <Frederic-Gerald Morcos>
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

#ifndef __PIECE
#define __PIECE

#include "cube.h"

/* defined standard colors */
#define I_R 1.0
#define I_G 0.0
#define I_B 0.0

#define T_R 192.0/255.0
#define T_G 192.0/255.0
#define T_B 192.0/255.0

#define Q_R 0.0
#define Q_G 1.0
#define Q_B 1.0

#define J_R 1.0
#define J_G 1.0
#define J_B 0.0

#define L_R 1.0
#define L_G 0.0
#define L_B 1.0

#define S_R 0.0
#define S_G 0.0
#define S_B 1.0

#define Z_R 0.0
#define Z_G 1.0
#define Z_B 0.0

enum _piece_type
{
	I = 0,
	Q = 1,
	S = 2,
	Z = 3,
	T = 4,
	L = 5,
	J = 6
};

typedef enum _piece_type piece_type;

enum _piece_rotation
{
	UP = 0,
	RIGHT = 1,
	LEFT = 2,
	DOWN = 3
};

typedef enum _piece_rotation piece_rotation;

struct _piece
{
	cube *list [4];
	int x, y;
	piece_type type;
	piece_rotation rotation;
};

typedef struct _piece piece;

piece *piece_new (int nx, int ny, piece_type nt);
void piece_set_position (piece *p, int nx, int ny);
void piece_draw (piece *p);
inline void piece_set_type (piece *p, piece_type nt);
void piece_move_right (piece *p);
void piece_move_left (piece *p);
boolean piece_move_down (piece *p);
inline piece_type get_random_type (void);
void piece_unload (piece *p);

#endif
