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

#import "container.h"

#define TC(w) GTK_CONTAINER(w)

@implementation Container

- add: (Widget *) wid {
	if (widget == NULL) return self;
	gtk_container_add(TC(widget), [wid widget]);
	return self;
}

- remove: (Widget *) wid {
	if (widget == NULL) return self;
	gtk_container_remove(TC(widget), [wid widget]);
	return self;
}

- borderWidth: (int) size {
	if (widget == NULL) return self;
	gtk_container_set_border_width(TC(widget), size);
	return self;
}

- (int) borderWidth {
	if (widget == NULL) return -1;
	return gtk_container_get_border_width(TC(widget));
}

@end

