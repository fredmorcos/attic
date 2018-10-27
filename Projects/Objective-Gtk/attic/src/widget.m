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

#import "widget.h"

#define TC(w) GTK_WIDGET(w)

@implementation Widget

- onDestroy: (Callback) cb: (gpointer) data {
	g_signal_connect(G_OBJECT(widget), "destroy", 
				G_CALLBACK(cb), data);
	return self;
}

- onExpose: (Callback) cb: (gpointer) data {
	g_signal_connect(G_OBJECT(widget), "expose-event",
				G_CALLBACK(cb), data);
	return self;
}

- onConfigure: (Callback) cb: (gpointer) data {
	g_signal_connect(G_OBJECT(widget), "configure-event",
				G_CALLBACK(cb), data);
	return self;
}

- onButtonPress: (Callback) cb: (gpointer) data {
	gtk_widget_add_events(TC(widget), GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(widget), "button-press-event", 
				G_CALLBACK(cb), data);
	return self;
}

- onButtonRelease: (Callback) cb: (gpointer) data {
	gtk_widget_add_events(TC(widget), GDK_BUTTON_RELEASE_MASK);
	g_signal_connect(G_OBJECT(widget), "button-release-event", 
				G_CALLBACK(cb), data);
	return self;
}

- onMotionNotify: (Callback) cb: (gpointer) data {
	gtk_widget_add_events(TC(widget), GDK_POINTER_MOTION_MASK);
	g_signal_connect(G_OBJECT(widget), "motion-notify-event", 
				G_CALLBACK(cb), data);
	return self;
}

- onDragMotion: (Callback) cb: (gpointer) data {
	g_signal_connect(G_OBJECT(widget), "drag-motion",
				G_CALLBACK(cb), data);
	return self;
}

- (GtkWidget *) widget {
	return widget;
}

- showAll {
	gtk_widget_show_all(TC(widget));
	return self;
}

- queueDraw {
	gtk_widget_queue_draw(TC(widget));
	return self;
}

- doubleBuffered: (BOOL) val {
	gtk_widget_set_double_buffered(TC(widget), val);
	return self;
}

- redrawOnAllocate: (BOOL) val {
	gtk_widget_set_redraw_on_allocate(TC(widget), val);
	return self;
}

- sensitive: (BOOL) val {
	gtk_widget_set_sensitive(TC(widget), val);
	return self;
}

@end

