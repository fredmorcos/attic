/*	
 *	This file is part of Objective-Gtk.
 *
 *	Copyright 2008, 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	Objective-Gtk is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Objective-Gtk is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Objective-Gtk.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "Widget.h"

@implementation Widget

- show {
	gtk_widget_show(widget);
	return self;
}

- showAll {
	gtk_widget_show_all(widget);
	return self;
}

- hide {
	gtk_widget_hide(widget);
	return self;
}

- hideAll {
	gtk_widget_hide_all(widget);
	return self;
}

@end

