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

#ifndef _CALLBACKS_H_
#define _CALLBACKS_H_

/* function prototypes for callbacks.c */

void btn_tool_clicked (GtkWidget *widget, GtkLabel *label);
void current_color_set (GtkWidget *widget);
void btn_new_clicked (void);
void btn_open_clicked (void);
void btn_save_clicked (void);
void btn_undo_clicked (void);
void btn_redo_clicked (void);

#endif
