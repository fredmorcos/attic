/*
	This file is part of imageman.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	imageman is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	imageman is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with imageman.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __CALLBACKS_H__
#define __CALLBACKS_H__

#include <gtk/gtk.h>

void menuAbout_activate(GtkMenuItem *, gpointer);
void menuOpen_activate(GtkMenuItem *, gpointer);
void menuSave_activate(GtkMenuItem *, gpointer);
void negateButton1_click(GtkButton *, gpointer);
void negateButton3_click(GtkButton *, gpointer);
void negateButton2_click(GtkButton *, gpointer);
void rotateButton_click(GtkButton *, gpointer);
void rotateButtonCV_click(GtkButton *, gpointer);
void upsampleButton_click(GtkButton *, gpointer);
void upsampleButtonCV_click(GtkButton *, gpointer);
void scaleButton_click(GtkButton *, gpointer);
void replicationRadio_toggle(GtkToggleButton *, gpointer);

#endif	/* __CALLBACKS_H__ */

