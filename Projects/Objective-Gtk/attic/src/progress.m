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

#import "progress.h"

#define TC(w) GTK_PROGRESS(w)

@implementation Progress

- showText: (BOOL) val {
	gtk_progress_set_show_text(TC(widget), val);
	return self;
}

- value: (double) val {
	gtk_progress_set_value(TC(widget), val);
	return self;
}

- (double) value {
	return gtk_progress_get_value(TC(widget));
}

- configure: (double) value: (double) min: (double) max {
	gtk_progress_configure(TC(widget), value, min, max);
	return self;
}

@end

