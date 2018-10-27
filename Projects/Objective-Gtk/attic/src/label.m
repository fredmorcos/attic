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

#import "label.h"

#define TC(w) GTK_LABEL(w)

@implementation Label

- init {
	self = [super init];
	if (self) widget = gtk_label_new(NULL);
	return self;
}

- free {
	gtk_widget_destroy(widget);
	return [super free];
}

- (const char *) text {
	return gtk_label_get_text(TC(widget));
}

- text: (const char *) val {
	gtk_label_set_text(TC(widget), val);
	return self;
}

@end

