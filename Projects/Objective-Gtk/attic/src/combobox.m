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

#import "combobox.h"

#define TC(w) GTK_COMBO_BOX(w)

@implementation ComboBox

- init {
	self = [super init];
	if (self) widget = gtk_combo_box_new_text();
	return self;
}

- free {
	gtk_widget_destroy(widget);
	return [super free];
}

- appendText: (const char *) text {
	gtk_combo_box_append_text(TC(widget), text);
	return self;
}

- (char *) activeText {
	return gtk_combo_box_get_active_text(TC(widget));
}

- setActive: (int) index {
	gtk_combo_box_set_active(TC(widget), index);
	return self;
}

- onChanged: (Callback) cb: (id) data {
	if (widget) {
		g_signal_connect(G_OBJECT(widget), "changed", 
				G_CALLBACK(cb), data);
	}
	return self;
}

@end

