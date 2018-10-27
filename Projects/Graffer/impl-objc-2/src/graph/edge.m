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

#import "edge.h"
#import "point.h"
#import <cairo.h>

@implementation Edge

- expose: (cairo_t *) context {
	cairo_set_source_rgb(context, 0.0, 0.0, 0.0);
	cairo_move_to(context, pointA->x, pointA->y);
	cairo_line_to(context, pointB->x, pointB->y);
	cairo_stroke(context);

	return self;
}

- (Point *) pointA {
	return pointA;
}

- (Point *) pointB {
	return pointB;
}

- pointA: (Point *) pa {
	pointA = pa;
	return self;
}

- pointB: (Point *) pb {
	pointB = pb;
	return self;
}

@end

