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

#import "togglebutton.h"

#define TC(w) GTK_TOGGLE_BUTTON(w)

@implementation ToggleButton

- init {
	self = [super init];
	if (self) widget = gtk_toggle_button_new();
	return self;
}

- (BOOL) active {
	return gtk_toggle_button_get_active(TC(widget));
}

- active: (BOOL) val {
	if (widget) gtk_toggle_button_set_active(TC(widget), val);
	return self;
}

- onToggled: (Callback) cb: (gpointer) data {
	if (widget)
		g_signal_connect(G_OBJECT(widget), "toggled", 
				G_CALLBACK(cb), data);
	return self;
}

@end

