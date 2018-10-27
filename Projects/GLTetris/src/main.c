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

#include <GL/glut.h>
#include <stdlib.h>
#include <stdio.h>

#include "global.h"
#include "grid.h"
#include "piece.h"
#include "well.h"

void init_window (void);
void cb_display (void);
void cb_special (int key, int x, int y);
void cb_timer (int val);
void cb_exit (void);
void cb_keyboard (unsigned char key, int x, int y);
void run_game (void);
void draw_text (int x, int y, char *s);
void cb_game_over (void);

grid *main_grid = NULL;
piece *current = NULL, *next = NULL;
boolean change = FALSE, pause = FALSE, gameover = FALSE;
well *main_well = NULL;
int level = 1, timer = 1000;

int main (int argc, char *argv[])
{
	/* initialize some tetris objects */
	main_grid = grid_new ();
	next = piece_new (12, 2, get_random_type ());
	current = piece_new (4, -1, get_random_type ());
	main_well = well_new ();

	atexit (cb_exit);
	
	/* initialize some glut objects */
	glutInit (&argc, argv);
	glutInitDisplayMode (GLUT_DOUBLE);
	glutInitWindowSize (WIN_WID, WIN_HEI);
	glutInitWindowPosition (200, 20);
	glutCreateWindow ("Tetris");

	glutSpecialFunc(cb_special);
	glutKeyboardFunc (cb_keyboard);
	glutDisplayFunc (cb_display);
	glutIdleFunc (cb_display);
	init_window ();
	run_game ();
	glutMainLoop ();
	
	return 0;
}

/* called if a piece hits the top bar */
void cb_game_over (void)
{
	gameover = TRUE;
	glutPostRedisplay ();
}

/* will run the game:
 * 		1. will check if there are any unremoved lines and remove them.
 * 		2. will check if the current piece can be put into the well.
 * 		3. if not, keep it falling.
 * 		4. if so, add it and reset the current and next piece.
 * 		5. update some stuff (timer interval depending on level, etc...)
 * 		6. call the timer to wait until we can let the piece fall again.
 */
void run_game ()
{	
	if (pause == FALSE && gameover == FALSE)
	{
		if (well_add_piece (main_well, current, cb_game_over) == TRUE)
		{
			piece_set_position (current, 4, -1);
			piece_set_type (current, next->type);
			piece_set_type (next, get_random_type ());
			current->rotation = UP;
			next->rotation = UP;
			level = (main_well->removed_lines / 5) + 1;
		}
		
		if (well_check_move_down (main_well, current) == TRUE)
			piece_move_down (current);
		
		if (timer <= 50) timer = 50;
		else timer = 1000 - (level * 50);
		glutTimerFunc (timer, cb_timer, 0);
	}
	
	glutPostRedisplay ();
}

void cb_timer (int val)
{	
	if (pause == FALSE && gameover == FALSE)
		run_game ();
}

/* our display callback, draw everything here :) */
void cb_display ()
{
	glClear (GL_COLOR_BUFFER_BIT);
	
	char lines [24], levels [24];
	sprintf (lines, "Lines: %d", main_well->removed_lines);
	sprintf (levels, "Level: %d", level);
	
	grid_draw (main_grid);
	
	if (pause == FALSE && gameover == FALSE)
		piece_draw (current);
	
	piece_draw (next);
	well_draw (main_well);
	draw_text (next->x - 1, next->y - 1, "Next Piece:");
	draw_text (next->x - 1, next->y + 5, lines);
	draw_text (next->x - 1, next->y + 6, levels);
	draw_text (next->x - 1, next->y + 8, "Press p to pause");
	draw_text (next->x - 1, next->y + 9, "Press q to quit");
	if (pause == TRUE || gameover == TRUE)
	{
		glColor4f(1.0, 1.0, 1.0, 0.8);
		glRecti (1, 0, GRID_COLS * GRID_STEP, GRID_ROWS * GRID_STEP - 1);
		if (pause == TRUE)
			draw_text (4, 9, "Paused");
		else if (gameover == TRUE)
			draw_text (4, 9, "Game Over");
	}
	
	glutSwapBuffers ();
}

/* to put text on the window at a grid location */
void draw_text (int x, int y, char *s)
{
	glColor3f (0.0, 0.0, 0.0);
	glRasterPos2i (x * GRID_STEP, y * GRID_STEP);
	glutBitmapString (GLUT_BITMAP_9_BY_15, s);
}

/* called when exiting */
void cb_exit ()
{
	free (main_grid);
	piece_unload (current);
	free (current);
	piece_unload (next);
	free (next);
	well_unload (main_well);
	free (main_well);
}

/* pause and exit keyboard functions */
void cb_keyboard (unsigned char key, int x, int y)
{
	if (key == 'p' && !gameover)
	{
		if (pause == TRUE)
		{
			pause = FALSE;
			run_game ();
		}
		else
			pause = TRUE;
	}
	else if (key == 'q')
		exit (0);
	
	glutPostRedisplay ();
}

/* to get the special arrow keys */
void cb_special (int key, int x, int y)
{
	if (pause == FALSE && gameover == FALSE)
	{
		if (key == GLUT_KEY_LEFT)
		{
			if (well_check_move_left (main_well, current) == TRUE)
				piece_move_left (current);
		}
		else if (key == GLUT_KEY_RIGHT)
		{
			if (well_check_move_right (main_well, current) == TRUE)
				piece_move_right (current);
		}
		else if (key == GLUT_KEY_UP)
		{
			// piece_rotate (current);
		}
		else if (key == GLUT_KEY_DOWN)
		{
			if (well_check_move_down (main_well, current) == TRUE)
				piece_move_down (current);
		}
		glutPostRedisplay ();
	}
}

/* initialize some opengl stuff for our window */
void init_window ()
{
	glEnable (GL_BLEND);
	glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glClearColor (1.0, 1.0, 1.0, 0.0);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	gluOrtho2D (0.0, WIN_WID, WIN_HEI, 0.0);
}
