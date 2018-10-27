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

#include "ground.h"

#include <GL/glut.h>
#include <stdlib.h>
#include "global.h"

inline void renderGround (ground *g)
{
	glPushMatrix ();
	glColor3d (0, 1, 0);
	glTranslated (0, -(g->iHeight), 0);
	glScaled (g->iWidth * CELL_SIZE, g->iHeight, g->iDepth * CELL_SIZE);
	glutSolidCube (1);
	glPopMatrix ();
}

ground *newGround (int iw, int ih, int id)
{
	ground *temp = calloc (1, sizeof (ground));
	temp->iWidth = iw;
	temp->iHeight = ih;
	temp->iDepth = id;
	
//	temp->uiTextureId = textureFile ("images/floor.raw", 128, 128);
	
	return temp;
}

