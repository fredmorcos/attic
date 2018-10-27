/*
 * This file is part of GasPaint
 * 
 * GasPaint OpenGL/Gtk+ based paint-like program.
 * Copyright (C) 2008  	Frederic-Gerald Morcos
 						Andrew Botros Boktor
 						Marleine Mounir Daoud
 * 
 * GasPaint is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * GasPaint is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GasPaint.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _DRAWING_H_
#define _DRAWING_H_

#include <GL/gl.h>
#include <GL/glut.h>
#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

/* Font Type and Size */
#define FONT_TYPE_AND_SIZE GLUT_BITMAP_9_BY_15
/* IMPORTANT: change this according to above macro */
#define FONT_H 9
#define FONT_W 15

#define DEG_TO_RAD (3.14)/180

void drawText ( int x, int y, char *p );
void drawString ( int x, int y, char *string );
void drawCircle ( int x0, int y0, int x1, int y1 );
void drawRect ( int x0, int y0, int x1, int y1);
void drawLine ( int x0, int y0, int x1, int y1);

/* Old Filling function signature, don't delete */
//void fill ( GdkColor initial, GdkColor final, int x, int y );

void fill ( int x, int y );

void drawCommand ( char *command );

#endif
