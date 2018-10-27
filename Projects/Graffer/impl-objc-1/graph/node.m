/*
	This file is part of Grafer.
	
	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License version 3 
	as published by	the Free Software Foundation.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer. If not, see <http://www.gnu.org/licenses/>.
*/

#import "node.h"
#import <cairo.h>
#import <math.h>

@implementation Node

- init {
	self = [super init];
	if (self) {
		velocity = [[Point alloc] init];
		netforce = [[Point alloc] init];
	}
	return self;
}

- free {
	[velocity free];
	[netforce free];
	return [super free];
}

- expose: (cairo_t *) context {
	if (locked)
		cairo_set_source_rgb(context, 1.0, 0.0, 0.0);
	else
		cairo_set_source_rgb(context, 0.0, 0.0, 0.0);

	cairo_arc(context, x, y, size / 6, 0, 2 * M_PI);
	cairo_fill(context);

	return self;
}

- (int) size {
	return size;
}

- size: (int) val {
	size = val;
	return self;
}

- (Point *) velocity {
	return velocity;
}

- (Point *) netforce {
	return netforce;
}

- lock: (BOOL) val {
	locked = val;
	return self;
}

- (BOOL) lock {
	return locked;
}

@end

