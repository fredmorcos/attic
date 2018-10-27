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

#import "Builder.h"

@implementation Builder

- initFromFile: (NSString *) filename {
	self = [super init];
	builder = gtk_builder_new();
	gtk_builder_add_from_file(builder, [filename cString], NULL);
	return self;
}

- (GtkObject *) getObjectWithName: (NSString *) name {
	return GTK_OBJECT(gtk_builder_get_object(builder, [name cString]));
}

- (void) dealloc {
	g_object_unref(G_OBJECT(builder));
	[super dealloc];
}

@end

