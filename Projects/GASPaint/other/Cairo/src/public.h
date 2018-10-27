/*
 * This file is part of gaspaint
 * 
 * Copyright (C) 2007-2009  Frederic-Gerald Morcos
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

#ifndef _PUBLIC_H_
#define _PUBLIC_H_

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

/* declares some public stuff */

#define LINE 0
#define RECT 1
#define CIRC 2
#define POLY 3
#define FILL 4

GtkWidget *main_window;

GtkWidget *draw_area;
GdkGLConfig *draw_config;

GdkColor current_color;

gint current_tool;
gint current_x0, current_y0, current_x1, current_y1;
gint num_clicks;

GString *events;

#endif
