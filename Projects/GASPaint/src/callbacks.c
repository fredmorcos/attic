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
#include <string.h>

#include "callbacks.h"
#include "ui.h"
#include "public.h"
#include "drawarea.h"

/* FOR MORE INFORMATION REFER TO THE GTK+ DOCUMENTATION */

/* when any of the tool buttons (line, rectangle, etc...) is clicked,
 * this function is called... it checks which button was clicked and
 * sets the current_tool var (in public.h) to the corresponding
 * tool so draw_area_clicked can then use it
 */
void btn_tool_clicked (GtkWidget *widget, GtkLabel *label)
{
	const gchar *name = gtk_button_get_label (GTK_BUTTON (widget));
	
	if (strcmp (name, "_Line") == 0)
	{
		num_clicks = 0;
		current_tool = LINE;
		gtk_label_set_label (label, "Line");
	}
	else if (strcmp (name, "_Rectangle") == 0)
	{
		num_clicks = 0;
		current_tool = RECT;
		gtk_label_set_label (label, "Rectangle");
	}
	else if (strcmp (name, "_Circle") == 0)
	{
		num_clicks = 0;
		current_tool = CIRC;
		gtk_label_set_label (label, "Circle");
	}
	else if (strcmp (name, "_Polygon") == 0)
	{
		if (current_tool == POLY)
		{
			num_clicks = 0;
			current_tool = -1;
			gtk_label_set_label (label, "Select Tool");
		}
		else
		{
			current_tool = POLY;
			gtk_label_set_label (label, "Polygon");
		}
	}
	else if (strcmp (name, "_Filler") == 0)
	{
		num_clicks = 0;
		current_tool = FILL;
		gtk_label_set_label (label, "Filler");
	}
}

/* called when the color has been changed by the user
 * sets a new color into a 16-bit integer (#FFFFFF format)
 * and adds an event of changing color
 */
void current_color_set (GtkWidget *widget)
{
	gtk_color_button_get_color (GTK_COLOR_BUTTON (widget), &current_color);
	gchar *newcolor = gdk_color_to_string (&current_color);
	
	char new_col [8];
	new_col [0] = '#';
	new_col [1] = newcolor [1];
	new_col [2] = newcolor [2];
	new_col [3] = newcolor [5];
	new_col [4] = newcolor [6];
	new_col [5] = newcolor [9];
	new_col [6] = newcolor [10];
	new_col [7] = '\0';
	
	char command [25];
	
	sprintf (command, "color %s", new_col);
	add_event (command);
}

/* called when the new button is clicked
 * clears everything, the events string and all other used vars
 */
void btn_new_clicked ()
{
	current_x0 = -1;
	current_y0 = -1;
	current_x1 = -1;
	current_y1 = -1;
	num_clicks = 0;
	events = g_string_erase (events, 0, events->len);
	draw_area_expose ();
}

/* open file, it shows an open dialog and then loads the file into the events string */
void btn_open_clicked ()
{
	gchar *filename;
	gchar *filedata;
	GtkWidget *dialog = gtk_file_chooser_dialog_new ("Open File...", GTK_WINDOW (main_window), 
														GTK_FILE_CHOOSER_ACTION_OPEN,
														GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
														GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
														NULL);
	
	
	gchar* current_path_c = gtk_file_chooser_get_current_folder_uri ( GTK_FILE_CHOOSER (dialog) );
	
	GString* current_path = g_string_new ( current_path_c );
	
	gchar* rel_path = "/save";
	g_string_append ( current_path, rel_path );
	
	gtk_file_chooser_set_current_folder_uri ( GTK_FILE_CHOOSER (dialog), current_path->str );
	
	
	
	
	gint result = gtk_dialog_run (GTK_DIALOG (dialog));
	if (result == GTK_RESPONSE_ACCEPT)
	{
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		btn_new_clicked ();
		if (g_file_get_contents (filename, &filedata, NULL, NULL) == TRUE)
		{
			events = g_string_assign (events, filedata);
			drawEvents ();
		}
		else
		{
			printf ("Error opening file...\n");
		}
	}
	
	gtk_widget_destroy (dialog);
}

/* save file, opens a file and loads the event string into it for saving */
void btn_save_clicked ()
{
	gchar *filename;
	GtkWidget *dialog = gtk_file_chooser_dialog_new ("Save File...", GTK_WINDOW (main_window),
														GTK_FILE_CHOOSER_ACTION_SAVE,
														GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
														GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
														NULL);
	
	gchar *current_path_c = gtk_file_chooser_get_current_folder_uri ( GTK_FILE_CHOOSER (dialog) );
	
	GString *current_path = g_string_new ( current_path_c );
	
	gchar *rel_path = "/save";
	g_string_append ( current_path, rel_path );
	
	gtk_file_chooser_set_current_folder_uri ( GTK_FILE_CHOOSER (dialog), current_path->str );
	
	gint result = gtk_dialog_run (GTK_DIALOG (dialog));
	if (result == GTK_RESPONSE_ACCEPT)
	{
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		if (g_file_set_contents (filename, events->str, events->len, NULL) != TRUE)
		{
			printf ("Error saving file...\n");
		}
	}
	
	gtk_widget_destroy (dialog);
}

/* when undo is clicked */
void btn_undo_clicked ()
{
}

/* when redo is clicked */
void btn_redo_clicked ()
{
}
