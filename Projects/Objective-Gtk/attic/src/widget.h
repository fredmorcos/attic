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

#import "object.h"

typedef void (*Callback)(void);

@interface Widget : GTKObject {
@protected
	GtkWidget *widget;
}

- onDestroy: (Callback) cb: (gpointer) data;
- onExpose: (Callback) cb: (gpointer) data;
- onConfigure: (Callback) cb: (gpointer) data;
- onButtonPress: (Callback) cb: (gpointer) data;
- onButtonRelease: (Callback) cb: (gpointer) data;
- onMotionNotify: (Callback) cb: (gpointer) data;
- onDragMotion: (Callback) cb: (gpointer) data;
- (GtkWidget *) widget;
- showAll;
- queueDraw;
- doubleBuffered: (BOOL) val;
- redrawOnAllocate: (BOOL) val;
- sensitive: (BOOL) val;

@end

