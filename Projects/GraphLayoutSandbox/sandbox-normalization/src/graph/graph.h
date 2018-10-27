#import <objc/Object.h>
#import "point.h"
#import "node.h"
#import "edge.h"
#import "gravity.h"
#import <list.h>
#import <glib.h>
#import <cairo.h>

@interface Graph : Object {
@protected
	List *nodeList, *edgeList;
	int width, height;
	Gravity *gravityNode;
	BOOL wall, gravity;
	int minMass;
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
- normalizeMass;
- addEdge: (Edge *) edge;
- (Node *) getNodeByPos: (int) x: (int) y;
- (BOOL) wall;
- wall: (BOOL) val;
- gravity: (int) size: (BOOL) enable;
- (BOOL) gravity;
- (Gravity *) gravityNode;
- (Point *) centerOfMass;

@end

