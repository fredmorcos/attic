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

#import <objc/Object.h>
#import "point.h"
#import <cairo.h>

@interface Edge : Object {
@protected
	Point *pointA, *pointB;
}

- expose: (cairo_t *) context;
- (Point *) pointA;
- (Point *) pointB;
- pointA: (Point *) pa;
- pointB: (Point *) pb;

@end

