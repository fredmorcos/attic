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
#import "graph.h"
#import "node.h"

@interface Nodes: Object {
}

+ nodeRandom: (Node *) node: (int) width: (int) height;
+ edgesCircular: (Graph *) graph;
+ edgesCentered: (Graph *) graph;
+ edgesInterconnected: (Graph *) graph;
+ edgesBinaryTree: (Graph *) graph;

@end

