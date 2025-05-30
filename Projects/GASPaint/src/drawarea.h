/*
 * This file is part of gaspaint
 * 
 * Copyright (C) 2007-2009  Frederic-Gerald Morcos
 * 							Andrew Botros
 *							Marleine Daoud
 *
 * gaspaint is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * gaspaint is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with gaspaint.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _DRAWAREA_H_
#define _DRAWAREA_H_

/* function prototypes for drawarea.c */

void draw_area_configure (void);
void draw_area_expose (void);
void draw_area_clicked (GtkWidget *widget, GdkEventButton *event);
void draw_area_motion (GtkWidget *widget, GdkEventMotion *event, GtkWidget *label);

void draw_area_set_color (void);

#endif
