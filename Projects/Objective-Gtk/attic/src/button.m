/*
	This file is part of Objective-Gtk.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Objective-Gtk is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Objective-Gtk is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Objective-Gtk.  If not, see <http://www.gnu.org/licenses/>.
*/

#import "button.h"

#define TC(w) GTK_BUTTON(w)

@implementation Button

- init {
	if ((self = [super init])) widget = gtk_button_new();
	return self;
}

- free {
	gtk_widget_destroy(widget);
	return [super free];
}

- (const char *) label {
	return gtk_button_get_label(TC(widget));
}

- label: (const char *) lab {
	gtk_button_set_label(TC(widget), lab);
	return self;
}

- stock: (BOOL) set {
	gtk_button_set_use_stock(TC(widget), set);
	return self;
}

- onClicked: (Callback) cb: (gpointer) data {
	g_signal_connect(G_OBJECT(widget), "clicked", G_CALLBACK(cb), data);
	return self;
}

@end

