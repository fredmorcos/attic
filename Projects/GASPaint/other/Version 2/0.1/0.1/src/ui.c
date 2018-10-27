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

#include <gtk/gtk.h>

#include "ui.h"
#include "callbacks.h"
#include "public.h"
#include "main.h"
#include "drawarea.h"
#include "event.h"

/* implements the gui and all registrations to callbacks in callbacks.c 
 * see the gtk documentation for this
 */

/* initializes the widgets for the main window */
void init_main_window (GtkWidget *win)
{
	GtkWidget *hbox, *vbox, *hsep1, *hsep2, *hsep3, *hsep4;
	GtkWidget *btn_line, *btn_rect, *btn_circ, *btn_poly, *btn_fill;
	GtkWidget *lbl_tool, *lbl_pos;
	GtkWidget *btn_col;
	GtkWidget *btn_new, *btn_open, *btn_save, *btn_undo, *btn_redo;
	GtkWidget *lbl_preset, *btn_preset_open, *btn_preset_save, *btn_preset;
	GtkWidget *btn_quit;
	
	gdk_color_parse ("#000000", &current_color);				/* sets the current color */
	add_event ("color #000000");

	btn_line = gtk_button_new_with_mnemonic ("_Line");			/* line button */
	btn_rect = gtk_button_new_with_mnemonic ("_Rectangle");		/* rectangle button */
	btn_circ = gtk_button_new_with_mnemonic ("_Circle");		/* circle button */
	btn_poly = gtk_button_new_with_mnemonic ("_Polygon");		/* polygon button */
	btn_fill = gtk_button_new_with_mnemonic ("_Filler");		/* filling button */
	lbl_tool = gtk_label_new ("Select Tool");					/* selected tool label */
	lbl_pos = gtk_label_new ("X: , Y:");						/* position label */
	hsep1 = gtk_hseparator_new ();								/* separator */
	btn_col = gtk_color_button_new_with_color (&current_color);	/* color button */
	hsep2 = gtk_hseparator_new ();								/* separator */
	btn_new = gtk_button_new_from_stock (GTK_STOCK_NEW);		/* new button */
	btn_open = gtk_button_new_from_stock (GTK_STOCK_OPEN);		/* open button */
	btn_save = gtk_button_new_from_stock (GTK_STOCK_SAVE);		/* save button */
	btn_undo = gtk_button_new_from_stock (GTK_STOCK_UNDO);		/* undo button */
	btn_redo = gtk_button_new_from_stock (GTK_STOCK_REDO);		/* redo button */
	hsep3 = gtk_hseparator_new ();								/* separator */
	// lbl_preset = gtk_label_new ("Preset:");						/* preset label */
	// btn_preset_open = gtk_file_chooser_button_new (				/* preset open button */
	//						"Choose Preset", 
	//						GTK_FILE_CHOOSER_ACTION_OPEN);
	// btn_preset = gtk_button_new_with_label ("Preset");			/* preset use button */
	// 	btn_preset_save = gtk_button_new_from_stock (				/* preset save button */
	//						GTK_STOCK_SAVE);
	// hsep4 = gtk_hseparator_new ();								/* separator */
	btn_quit = gtk_button_new_from_stock (GTK_STOCK_QUIT);		/* quit button */
	
	/* the drawing area */
	draw_area = gtk_drawing_area_new ();
	gtk_widget_set_events (draw_area, GDK_EXPOSURE_MASK | 
										GDK_BUTTON_PRESS_MASK | 
										GDK_POINTER_MOTION_MASK);
	gtk_widget_set_size_request (draw_area, 800, 600);
	
	/* the drawing config */
	draw_config = gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB | 
												GDK_GL_MODE_DEPTH | 
												GDK_GL_MODE_DOUBLE);
	
	/* if something is wrong, print a message and quit the application */
	if (!draw_config || !gtk_widget_set_gl_capability (draw_area, draw_config, NULL, TRUE, GDK_GL_RGBA_TYPE))
			g_assert_not_reached ();
	
	/* start: add the widgets to the left panel */
	vbox = gtk_vbox_new (FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_line);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_rect);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_circ);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_poly);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_fill);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), lbl_tool);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), lbl_pos);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), hsep1);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_col);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), hsep2);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_new);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_open);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_save);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_undo);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_redo);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), hsep3);
	// gtk_box_pack_start_defaults (GTK_BOX (vbox), lbl_preset);
	// gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_preset_open);
	// gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_preset);
	// gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_preset_save);
	// gtk_box_pack_start_defaults (GTK_BOX (vbox), hsep4);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), btn_quit);
		
	gtk_widget_set_size_request (vbox, 120, 600);
	/* end: add the widgets to the left panel */
	
	g_signal_connect (G_OBJECT (btn_line), "clicked", G_CALLBACK (btn_tool_clicked), lbl_tool);
	g_signal_connect (G_OBJECT (btn_rect), "clicked", G_CALLBACK (btn_tool_clicked), lbl_tool);
	g_signal_connect (G_OBJECT (btn_circ), "clicked", G_CALLBACK (btn_tool_clicked), lbl_tool);
	g_signal_connect (G_OBJECT (btn_poly), "clicked", G_CALLBACK (btn_tool_clicked), lbl_tool);
	g_signal_connect (G_OBJECT (btn_fill), "clicked", G_CALLBACK (btn_tool_clicked), lbl_tool);
	
	g_signal_connect (G_OBJECT (btn_col), "color-set", G_CALLBACK (current_color_set), NULL);
	
	g_signal_connect (G_OBJECT (btn_new), "clicked", G_CALLBACK (btn_new_clicked), NULL);
	g_signal_connect (G_OBJECT (btn_open), "clicked", G_CALLBACK (btn_open_clicked), NULL);
	g_signal_connect (G_OBJECT (btn_save), "clicked", G_CALLBACK (btn_save_clicked), NULL);
	
	g_signal_connect (G_OBJECT (btn_undo), "clicked", G_CALLBACK (btn_undo_clicked), NULL);
	g_signal_connect (G_OBJECT (btn_redo), "clicked", G_CALLBACK (btn_redo_clicked), NULL);
	
	g_signal_connect (G_OBJECT (draw_area), "configure-event", G_CALLBACK (draw_area_configure), NULL);
	g_signal_connect (G_OBJECT (draw_area), "button-press-event", G_CALLBACK (draw_area_clicked), NULL);
	g_signal_connect (G_OBJECT (draw_area), "expose-event", G_CALLBACK (draw_area_expose), NULL);
	g_signal_connect (G_OBJECT (draw_area), "motion-notify-event", G_CALLBACK (draw_area_motion), lbl_pos);
	
	g_signal_connect (G_OBJECT (btn_quit), "clicked", G_CALLBACK (quit_gaspaint), NULL);
	
	hbox = gtk_hbox_new (FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), vbox);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), draw_area);
	
	gtk_container_add (GTK_CONTAINER (win), hbox);
	
	gtk_widget_show_all (win);
	
	/* sets the drawing color of the draw area, why is it here? because the widget has to
	 * be realized before it can be used... 
	 */
	draw_area_set_color ();
}
