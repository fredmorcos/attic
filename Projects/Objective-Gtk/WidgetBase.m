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

#import "WidgetBase.h"

@implementation WidgetBase

- initWithName: (NSString *) name {
	self = [super init];
	builder = [[Builder alloc] initFromFile: name];
	widget = GTK_WIDGET([builder getObjectWithName: name]);
	[self initEvent];
	return self;
}

- initWithGtkObject: (GtkObject *) obj {
	self = [super init];
	widget = GTK_WIDGET(obj);
	[self initEvent];
	return self;
}

- initEvent {
	event = @"expose-event";
	return self;
}

- (void) dealloc {
	g_object_unref(G_OBJECT(widget));
	[event release];
	[builder dealloc];
	[super dealloc];
}

- setAction: (SEL) act {
	action = [self methodForSelector: @selector(act)];
	g_signal_connect(G_OBJECT(widget), [event cString], 
		G_CALLBACK(((struct { @defs(WidgetBase) } *) self)->action), NULL);
	return self;
}

- (GtkWidget *) widget {
	return widget;
}

- (WidgetBase *) childFromName: (NSString *) name {
	return [[WidgetBase alloc] initWithGtkObject: 
		GTK_OBJECT([builder getObjectWithName: name])];
}

@end

