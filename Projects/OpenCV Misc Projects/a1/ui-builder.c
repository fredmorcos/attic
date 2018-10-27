/*
	This file is part of cv-a1.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv-a1 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv-a1 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv-a1.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "ui-builder.h"
#include <gtk/gtk.h>

static GtkBuilder	*builder;

void ui_init() {
	builder = gtk_builder_new();
	gtk_builder_add_from_file(builder, "ui.xml", NULL);
}

void ui_destroy() {
	g_object_unref(G_OBJECT(builder));
}

GtkWidget *ui_get_widget(gchar *name) {
	return GTK_WIDGET(gtk_builder_get_object(builder, name));
}

GtkAction *ui_get_action(gchar *name) {
	return GTK_ACTION(gtk_builder_get_object(builder, name));
}

