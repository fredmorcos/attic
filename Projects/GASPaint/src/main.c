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

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>
#include <stdlib.h>

#include "main.h"
#include "ui.h"
#include "public.h"

/* initializes the main window and then calls init_main_window () to initialize
 * all of the chilld widgets
 */
int main (int argc, char *argv[])
{	
	num_clicks = 0;
	current_tool = -1;
	current_x0 = -1;
	current_y0 = -1;
	current_x1 = -1;
	current_y1 = -1;
	
	gtk_init (&argc, &argv);
	gtk_gl_init (&argc, &argv);
	
	main_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (main_window), "Gas Paint");
	gtk_container_set_border_width (GTK_CONTAINER (main_window), 5);
	gtk_widget_set_size_request (main_window, 900, 650);
	gtk_window_set_resizable (GTK_WINDOW (main_window), FALSE);
	
	g_signal_connect (G_OBJECT (main_window), "destroy", G_CALLBACK (quit_gaspaint), NULL);
	
	/* init the event string */
	events = g_string_new ("");
	
	init_main_window (main_window);
	gtk_main ();
	return 0;
}

/* will free some vars and exit the program */
void quit_gaspaint ()
{
	gdk_color_free (&current_color);
	gtk_main_quit ();
}
