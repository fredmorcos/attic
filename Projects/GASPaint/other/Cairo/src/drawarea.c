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

#include <gtk/gtk.h>
#include <string.h>

#include "drawarea.h"
#include "public.h"
#include "config.h"
#include "drawing.h"
#include "event.h"

/* our drawing area */

/* is called when the drawing area is clicked,
 * manages the various clicks for the various tools
 */
void draw_area_clicked (GtkWidget *widget, GdkEventButton *event)
{		
	gint x = event->x, y = event->y;
	
	if (num_clicks == 0)
	{
		num_clicks++;
		current_x0 = x;
		current_y0 = y;
		
		if (current_tool == FILL)
		{
			char command [25];
			num_clicks = 0;
			sprintf (command, "fill %d %d", current_x0, current_y0);
			add_event (command);
			draw_area_expose ();
		}
	}
	else if (num_clicks == 1)
	{
		num_clicks = 0;
		current_x1 = x;
		current_y1 = y;
		
		char command [25];		/* 3*4 digits + 4 (spaces) + 4 chars (command name) + \0 */
		
		if (current_tool == LINE)
		{
			sprintf (command, "line %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
			add_event (command);
			draw_area_expose ();
		}
		else if (current_tool == RECT)
		{
			sprintf (command, "rect %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
			add_event (command);
			draw_area_expose ();
		}
		else if (current_tool == CIRC)
		{
			sprintf (command, "circ %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
			add_event (command);
			draw_area_expose ();
		}
		else if (current_tool == POLY)
		{
			num_clicks = 1;
			sprintf (command, "line %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
			current_x0 = current_x1;
			current_y0 = current_y1;
			
			add_event (command);
			draw_area_expose ();
		}
	}
	else
	{
		current_x0 = -1;
		current_y0 = -1;
		current_x1 = -1;
		current_y1 = -1;
		num_clicks = 0;
	}
}

/* called when the mouse is moving over the drawing area */
void draw_area_motion (GtkWidget *widget, GdkEventMotion *event, GtkWidget *label)
{
	gint x = event->x, y = event->y;
	char pos [20];
	
	sprintf (pos, "X: %d, Y: %d", x, y);
	gtk_label_set_label (GTK_LABEL (label), pos); 
}

