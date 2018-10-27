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

#import "range.h"

#define TC(w) GTK_RANGE(w)

@implementation Range

- range: (double) min: (double) max {
	if (widget) 
		gtk_range_set_range(TC(widget), min, max);
	return self;
}

- value: (double) value {
	if (widget) 
		gtk_range_set_value(TC(widget), value);
	return self;
}

- (double) value {
	return gtk_range_get_value(TC(widget));
}

- increments: (double) step: (double) page {
	if (widget) 
		gtk_range_set_increments(TC(widget), step, page);
	return self;
}

- onValueChanged: (Callback) cb: (gpointer) data {
	if (widget)
		g_signal_connect(G_OBJECT(widget), "value-changed",
				G_CALLBACK(cb), data);
	return self;
}

@end

