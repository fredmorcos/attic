/*
 *      galarm-main-window.c
 *      
 *      Copyright 2007 Fred Morcos <fred.morcos@gmail.com>
 *      
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *      
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *      
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *      MA 02110-1301, USA.
 */


#include <stdio.h>

/* GUI (GTK+) */
#include <gtk/gtk.h>
#include "galarm-main.h"

int main (int argc, char *argv[])
{
	if (!gtk_init_check (&argc, &argv))
	{
		printf ("FAILED: gtk_init_check ()\nExiting.\n");
		return 0;
	}
	
	GtkWidget *window, *tree, *vbox, 
				*header_icon, *header_name, *header_desc, 
				*header_hbox, *header_labels_fixed;
	GtkTreeViewColumn *column;
	GtkCellRenderer *renderer;
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_CENTER);
	gtk_window_set_title (GTK_WINDOW (window), APP_NAM_VER);
	gtk_window_set_icon_from_file (GTK_WINDOW (window), 
									APP_ICO, 
									NULL);
	gtk_widget_set_size_request (window, 400, 200);
	gtk_container_set_border_width (GTK_CONTAINER (window), 5);					
	
	header_icon = gtk_image_new_from_file (APP_ICO);
	
	header_name = gtk_label_new ("GAlarm Events");
	gtk_label_set_markup (GTK_LABEL (header_name), 
							"<span size='x-large'><b>GAlarm Events</b></span>");
	header_desc = gtk_label_new ("Right-Click the list to manage your events");
	gtk_label_set_markup (GTK_LABEL (header_desc), 
							"<i>Right-Click the list to manage your events</i>");
	
	header_labels_fixed = gtk_fixed_new ();
	gtk_fixed_put (GTK_FIXED (header_labels_fixed), header_name, 0, 5);
	GtkRequisition *req;
	gtk_widget_size_request (header_name, req);
	gint name_label_height = req -> height;
	gtk_fixed_put (GTK_FIXED (header_labels_fixed), header_desc, 
					0, name_label_height + 5);
							
	header_hbox = gtk_hbox_new (FALSE, 5);
	gtk_box_pack_start (GTK_BOX (header_hbox), header_icon, FALSE, FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (header_hbox), header_labels_fixed);
	
	renderer = gtk_cell_renderer_text_new ();
	tree = gtk_tree_view_new ();
	column = gtk_tree_view_column_new_with_attributes ("Event", renderer, NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);
	
	vbox = gtk_vbox_new (FALSE, 5);
	gtk_box_pack_start (GTK_BOX (vbox), header_hbox, FALSE, FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), tree);
	
	gtk_container_add (GTK_CONTAINER (window), vbox);
	
	gtk_widget_show_all (window);
	gtk_main ();
	
	return 0;
}
