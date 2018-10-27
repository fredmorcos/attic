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

#import "point.h"
#import <cairo.h>

@interface Node : Point {
@protected
	Point *velocity, *netforce;
	int size;
	BOOL locked;
}

- init;
- free;

- expose: (cairo_t *) context;
- (int) size;
- size: (int) val;
- (Point *) velocity;
- (Point *) netforce;
- lock: (BOOL) val;
- (BOOL) lock;

@end

