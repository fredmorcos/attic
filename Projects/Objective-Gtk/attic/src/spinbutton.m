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

#import "spinbutton.h"

#define TC(w) GTK_SPIN_BUTTON(w)

@implementation SpinButton

- init {
	self = [super init];
	if (self) widget = gtk_spin_button_new(0, 0, 0);
	return self;
}

- free {
	gtk_widget_destroy(widget);
	return [super free];
}

- range: (double) min: (double) max {
	gtk_spin_button_set_range(TC(widget), min, max);
	return self;
}

- step: (double) step: (double) page {
	gtk_spin_button_set_increments(TC(widget), step, page);
	return self;
}

- value: (double) val {
	gtk_spin_button_set_value(TC(widget), val);
	return self;
}

- (int) valueInt {
	return gtk_spin_button_get_value_as_int(TC(widget));
}

- (double) valueDouble {
	return gtk_spin_button_get_value_as_float(TC(widget));
}

- onValueChanged: (Callback) cb: (id) data {
	if (widget)
		g_signal_connect(G_OBJECT(widget), "value-changed", 
				G_CALLBACK(cb), data);
	return self;
}

@end

