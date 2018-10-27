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

#include "glut-extra.h"

#include <GL/glut.h>
#include "opengl-render.h"

void initGlutWindow (int iWidth, int iHeight)
{
	glutInitWindowSize (iWidth, iHeight);
	glutInitWindowPosition ((glutGet (GLUT_SCREEN_WIDTH) - iWidth) / 2, 
							(glutGet (GLUT_SCREEN_HEIGHT) - iHeight) / 2);
	glutCreateWindow ("Pacman NG");
}

void initGlut (void (*cbDisplay) (void), void (*cbKeyboard) (unsigned char ucKey, int iX, int iY), void (*cbKeyboardSpecial) (int iKey, int iX, int iY))
{
	glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);	
	glutDisplayFunc (cbDisplay);
//	glutIdleFunc (cbDisplay);
//	glutReshapeFunc (resize);
	glutKeyboardFunc (cbKeyboard);
	glutSpecialFunc (cbKeyboardSpecial);	
}
