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

#include "bille.h"

#include <GL/glut.h>
#include <stdlib.h>
#include "global.h"

bille *newBille (int ix, int iy, int iz, boolean bv)
{
	bille *temp = calloc (1, sizeof (bille));
	temp->ix = ix;
	temp->iy = iy;
	temp->iz = iz;
	temp->bVisible = bv;
	return temp;
}

inline void renderBille (bille *b)
{
	if (!b->bVisible)
		return;
	
	glPushMatrix ();
	glColor3d (0, 1, 1);
	glTranslated (b->ix * CELL_SIZE, b->iy + (BILLE_RADIUS * 2), b->iz * CELL_SIZE);
	glutSolidSphere (BILLE_RADIUS, 10, 10);
	glPopMatrix ();
}

boolean set_bille_visibility (bille *pBilles [85], int x, int z, int *iScore)
{
	int i = 0; 
	boolean bWin = TRUE;
	while (i < 85)
	{
		if(pBilles [i] != NULL && pBilles[i]->iz == z && pBilles[i]->ix == x)
		{
			pBilles[i]->bVisible = FALSE;
			(*iScore) += 10;
		}
		
		if (pBilles [i] != NULL && (pBilles [i])->bVisible == TRUE)
			bWin = FALSE;
		i++;
	}
	return bWin;
}
