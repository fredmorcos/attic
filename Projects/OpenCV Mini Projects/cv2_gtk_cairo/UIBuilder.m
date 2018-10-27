/*
	This file is part of cv1-gtk-cairo.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv1-gtk-cairo is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv1-gtk-cairo is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv1-gtk-cairo.  If not, see <http://www.gnu.org/licenses/>.
*/

#import "UIBuilder.h"
#import <gtk/gtk.h>

@implementation UIBuilder

/**
 * Adds a UI XML description from file.
 */
- (BOOL) addFromFile: (const char *) filename {
	if (!builder) builder = gtk_builder_new();
	return gtk_builder_add_from_file(builder, filename, NULL);
}

/**
 * Gets a widget named name.
 */
- (GtkWidget *) getWidget: (const char *) name {
	return GTK_WIDGET(gtk_builder_get_object(builder, name));
}

/**
 * UIBuilder destructor.
 */
- free {
	g_object_unref(builder);
	return [super free];
}

@end

