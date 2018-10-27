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
#import "node.h"
#import "edge.h"
#import <list.h>
#import <glib.h>
#import <cairo.h>

@interface Graph : Object {
@protected
	List *nodeList, *edgeList;
	int width, height;
	BOOL wall;
}

- init;
- free;

- expose: (cairo_t *) context;
- (List *) nodeList;
- (List *) edgeList;
- (int) width;
- (int) height;
- width: (int) w;
- height: (int) h;
- addNode: (Node *) node;
- addEdge: (Edge *) edge;
- (Node *) getNodeByPos: (int) x: (int) y;
- (BOOL) wall;
- wall: (BOOL) val;

@end

