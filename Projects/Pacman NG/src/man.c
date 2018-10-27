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

#include <GL/glut.h>
#include <stdlib.h>

#include "global.h"
#include "man.h"

man *newMan (float ix, float iy, float iz, boolean be)
{
	man *temp = calloc (1, sizeof (man));
	temp->ix = ix;
	temp->iy = iy;
	temp->iz = iz;
	temp->bEvil = be;
	
	return temp;
}

inline void renderMan (man *m)
{
	glPushMatrix ();
	if (m->bEvil)
		glColor3d (1, 0, 0);
	else
		glColor3d (1, 1, 0);
	
	glTranslatef (m->ix * CELL_SIZE, m->iy + (CELL_SIZE / 2), m->iz * CELL_SIZE);
	glutSolidSphere (CELL_SIZE / 2, 50, 50);
	glPopMatrix ();
}
