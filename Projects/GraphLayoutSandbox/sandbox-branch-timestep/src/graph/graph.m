#import "graph.h"
#import "edge.h"
#import "node.h"
#import <list.h>

@implementation Graph

- init {
	if ((self = [super init])) {
		nodeList = [[List alloc] init];
		edgeList = [[List alloc] init];
	}
	return self;
}

- free {
	[nodeList free];
	[edgeList free];
	return [super free];
}

- expose: (cairo_t *) context {
	int i = 0;
	int nsize = [nodeList size];
	int esize = [edgeList size];

	i = 0;
	while (YES) {
		if (i >= nsize && i >= esize) 
			break;

		if (i < esize)
			[[edgeList getItem: i] expose: context];

		if (i < nsize) 
			[[nodeList getItem: i] expose: context];

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
	[nodeList add: node];
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
		nx = n->x;
		ny = n->y;
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

@end

