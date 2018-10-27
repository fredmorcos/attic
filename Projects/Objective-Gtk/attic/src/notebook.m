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

#import "notebook.h"

#define TC(w) GTK_NOTEBOOK(w)

@implementation Notebook

- init {
	if ((self = [super init])) widget = gtk_notebook_new();
	return self;
}

- free {
	gtk_widget_destroy(widget);
	return [super free];
}

- (int) appendPage: (Widget *) child: (Widget *) label {
	return gtk_notebook_append_page(TC(widget), [child widget], [label widget]);
}

- tabPos: (GtkPositionType) pos {
	gtk_notebook_set_tab_pos(TC(widget), pos);
	return self;
}

- showBorder: (BOOL) val {
	gtk_notebook_set_show_border(TC(widget), val);
	return self;
}

@end

