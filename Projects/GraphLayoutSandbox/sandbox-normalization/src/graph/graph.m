#import "graph.h"
#import "edge.h"
#import "node.h"
#import <list.h>

@implementation Graph

- init {
	if ((self = [super init])) {
		nodeList = [[List alloc] init];
		edgeList = [[List alloc] init];
		gravityNode = [[Gravity alloc] init];
	}
	return self;
}

- free {
	[nodeList free];
	[edgeList free];
	[gravityNode free];
	return [super free];
}

- expose: (cairo_t *) context {
	int i = 0;
	int nsize = [nodeList size];
	int esize = [edgeList size];

	if (gravity) [gravityNode expose: context];

	i = 0;
	while (YES) {
		if (i >= nsize && i >= esize) 
			break;

		if (i < nsize) 
			[[nodeList getItem: i] expose: context];

		if (i < esize)
			[[edgeList getItem: i] expose: context];

		i++;
	}

	if (wall) {
		cairo_set_source_rgb(context, 0.5, 0.0, 0.0);
		cairo_set_line_width(context, 15.0);
		cairo_rectangle(context, 0, 0, width, height);
		cairo_stroke(context);
	}

	return self;
}

- (List *) nodeList {
	return nodeList;
}

- (List *) edgeList {
	return edgeList;
}

- (int) width {
	return width;
}

- (int) height {
	return height;
}

- width: (int) w {
	width = w;
	return self;
}

- height: (int) h {
	height = h;
	return self;
}

- addNode: (Node *) node {
	int noderealmass = [node size];

	if ([nodeList size] == 0) {
		minMass = noderealmass;
		[node mass: 1];
		[nodeList add: node];
	}
	else {
		if (minMass > noderealmass) {
			minMass = noderealmass;
			[nodeList add: node];
			[self normalizeMass];
		}
		else if (minMass < noderealmass) {
			[node mass: noderealmass / minMass];
			[nodeList add: node];
		}
		else {
			[node mass: 1];
			[nodeList add: node];
		}
	}

	if (gravity)
		[self gravity: [gravityNode size]: gravity];

	return self;
}

/* choose the smallest mass as the base for multiples */
- normalizeMass {
	Node *node;
	int i = 0;

	while (i < [nodeList size]) {
		node = [nodeList getItem: i];
		[node mass: [node size] / minMass];
		i++;
	}

	[gravityNode mass: [gravityNode size] / minMass];

	return self;
}

- addEdge: (Edge *) edge {
	[edgeList add: edge];
	return self;
}

- (Node *) getNodeByPos: (int) x: (int) y {
	int i = 0, nsize = 0, nx = 0, ny = 0;
	Node *n;

	while (i < [nodeList size]) {
		n = [nodeList getItem: i];
		nsize = [n size];
		nx = [n x];
		ny = [n y];
		if (x >= (nx - (nsize / 2)) &&
				x <= (nx + (nsize / 2)) &&
				y >= (ny - (nsize / 2)) &&
				y <= (ny + (nsize / 2)))
			return n;
		i++;
	}
	return NULL;
}

- (BOOL) wall {
	return wall;
}

- wall: (BOOL) val {
	wall = val;
	return self;
}

- gravity: (int) size: (BOOL) enable {
	gravity = enable;
	[gravityNode size: size];
	if (minMass != 0)
		[gravityNode mass: [gravityNode size] / minMass];

	if (gravity) {
		Point *p = [self centerOfMass];
		[[gravityNode x: [p x]] y: [p y]];
		[p free];
	}

	return self;
}

- (BOOL) gravity {
	return gravity;
}

- (Gravity *) gravityNode {
	return gravityNode;
}

- (Point *) centerOfMass {
	int x, y, i;
	int size = [nodeList size];

	i = 0;
	while (i < size) {
		x += [[nodeList getItem: i] x];
		y += [[nodeList getItem: i] y];
		i++;
	}

	if (size != 0)
		return [[[[Point alloc] init] x: x / size] y: y / size];

	return [[[[Point alloc] init] x: 0] y: 0];
}

@end

