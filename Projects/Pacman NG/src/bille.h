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

#ifndef __BILLE
#define __BILLE

#include "global.h"

struct _bille
{
	int ix, iy, iz;
	boolean bVisible;
};

typedef struct _bille bille;

bille *newBille (int ix, int iy, int iz, boolean bv);
void renderBille (bille *b);
boolean set_bille_visibility (bille *pBilles [85], int x, int z, int *iScore);

#endif

