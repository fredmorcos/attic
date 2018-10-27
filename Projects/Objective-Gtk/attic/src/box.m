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

#import "box.h"

#define TC(w) GTK_BOX(w)

@implementation Box

- packStart: (Widget *) child: (BOOL) expand: (BOOL) fill: (int) padding {
	if (widget == NULL) return self;
	gtk_box_pack_start(TC(widget), [child widget], expand, fill, padding);
	return self;
}

- packStart: (Widget *) child {
	return [self packStart: child: NO: NO: 0];
}

- homogeneous: (BOOL) homogen {
	if (widget == NULL) return self;
	gtk_box_set_homogeneous(TC(widget), homogen);
	return self;
}

- (BOOL) homogeneous {
	if (widget == NULL) return FALSE;
	return gtk_box_get_homogeneous(TC(widget));
}

- spacing: (int) space {
	if (widget == NULL) return self;
	gtk_box_set_spacing(TC(widget), space);
	return self;
}

- (int) spacing {
	if (widget == NULL) return -1;
	return gtk_box_get_spacing(TC(widget));
}

@end

