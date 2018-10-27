/*
 * This file is part of pacman-ng
 * 
 * Hex packet sniffer
 * Copyright (C) 2007  Frederic-Gerald Morcos
 * 
 * pacman-ng is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * pacman-ng is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with pacman-ng.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <GL/glut.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "glut-extra.h"
#include "global.h"
#include "opengl-render.h"
#include "ground.h"
#include "bille.h"
#include "wall.h"
#include "man.h"
#include "map.h"
#include "game.h"
#include "text.h"
#include "texture.h"

#define WIN_W 800
#define WIN_H 600

void initGame (void);
void moveEvil (int val);
void cbKeyboard (unsigned char ucKey, int iX, int iY);
void cbKeyboardSpecial (int iKey, int iX, int iY);
void cbDisplay (void);
void cbExit (void);

ground *pGround;
man *pPacman;
man *pEvil;
map *pMap;
bille *pBilles [85];
unsigned int displayList;
unsigned int texture;
boolean bGameOver = FALSE, bWin = FALSE;
int iScore = 0;
char cScoreNum [12] = "SCORE: 00000";

int main (int argc, char *argv [])
{
	atexit (cbExit);
	glutInit (&argc, argv);
	initGlutWindow (WIN_W, WIN_H);
	initGlut (cbDisplay, cbKeyboard, cbKeyboardSpecial);
	initOpenGL (WIN_W, WIN_H);
	initGame ();
	glutMainLoop ();
	return 0;
}

void moveEvil (int val)
{
	srand(time(NULL));
	int x = abs ((((time (NULL)) % 60) * rand ()) % 4);
	int check = 0;
	while (check == 0 && !bWin && !bGameOver)
	{
		x = rand() % 4;
		check = 0;
		switch (x)
		{
			case 0: 
				check = map_can_be_here (pMap, pEvil->ix+1, pEvil->iz);
				if (check) pEvil->ix++;
				break;
			case 1: 
				check = map_can_be_here (pMap, pEvil->ix, pEvil->iz+1);
				if (check) pEvil->iz++;
				break;
			case 2: 
				check = map_can_be_here (pMap, pEvil->ix-1, pEvil->iz);
				if (check) pEvil->ix--;
				break;
			case 3: 
				check = map_can_be_here (pMap, pEvil->ix, pEvil->iz-1);
				if (check) pEvil->iz--;
				break;
		}
	}
	
	glutPostRedisplay ();
	
	if (pPacman->ix == pEvil->ix && pPacman->iz == pEvil->iz)
		bGameOver = TRUE;
	
	if (!bGameOver && !bWin)
		glutTimerFunc (1000, moveEvil, 1);
}

void initGame ()
{
	pGround = newGround (13, 2, 13);
	pPacman = newMan (-1, 0, 5, FALSE);
	pMap = newMap ();
	pEvil = newMan (-2, 0, -2, TRUE);
	initMap (pMap);
	initBilles (pMap, pBilles, pPacman);
	moveEvil (1);
//	texture = LoadTextureRAW( "texture.raw", TRUE );
}

void testTexture ()
{
//	glEnable( GL_TEXTURE_2D );
//	glBindTexture( GL_TEXTURE_2D, texture );

	glPushMatrix();
	glColor3f (0.0, 1.0, 0.0);
	glScaled (60.0, 60.0, 60.0);
	glBegin( GL_QUADS );
	glTexCoord2d(0.0,0.0); glVertex3d(-1.0, 0.0, +1.0);
	glTexCoord2d(1.0,0.0); glVertex3d(+1.0, 0.0, +1.0);
	glTexCoord2d(1.0,1.0); glVertex3d(+1.0, 0.0, -1.0);
	glTexCoord2d(0.0,1.0); glVertex3d(-1.0, 0.0, -1.0);
	glEnd();
	glPopMatrix();
}

void cbDisplay ()
{
	renderOpenGL (pPacman->ix * CELL_SIZE, pPacman->iz * CELL_SIZE);
	testTexture ();
	renderGround (pGround);
	renderMap (pMap);
	renderMan (pPacman);
	renderMan (pEvil);
	renderBilles (pBilles);
	sprintf (cScoreNum, "SCORE: %d", iScore);
	if (!bGameOver && !bWin)
		renderText (cScoreNum);
	if (bGameOver && !bWin)
		renderText ("GAME OVER :\(");
	if (bWin)
		renderText ("YOU WIN!!!");
	glutSwapBuffers ();
}

void cbKeyboard (unsigned char ucKey, int iX, int iY)
{
	switch (ucKey)
	{
		case 'q':
//			glutLeaveMainLoop ();			
			exit (0);
			break;
	}
}

void cbKeyboardSpecial (int iKey, int iX, int iY)
{
	if (bGameOver == FALSE && bWin == FALSE)
	{
		switch (iKey)
		{
			case GLUT_KEY_UP:
				if (map_can_be_here (pMap, pPacman->ix, pPacman->iz - 1)) pPacman->iz--;
				break;
			case GLUT_KEY_DOWN:
				if (map_can_be_here (pMap, pPacman->ix, pPacman->iz + 1)) pPacman->iz++;
				break;
			case GLUT_KEY_LEFT:
				if (map_can_be_here (pMap, pPacman->ix - 1, pPacman->iz)) pPacman->ix--;
				break;
			case GLUT_KEY_RIGHT:
				if (map_can_be_here (pMap, pPacman->ix + 1, pPacman->iz)) pPacman->ix++;
				break;
		}
		
		if (pPacman->ix == pEvil->ix && pPacman->iz == pEvil->iz)
			bGameOver = TRUE;
		
		bWin = set_bille_visibility (pBilles, pPacman->ix, pPacman->iz, &iScore);
		glutPostRedisplay ();
	}
}

void cbExit ()
{
	/* free your stuff here, we're exiting */
	glDeleteTextures (1, &(pGround->uiTextureId));
	free (pGround);
	free (pPacman);
	free (pMap);
}
