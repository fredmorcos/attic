#import <objc/Object.h>
#import <graph.h>

#import "layout.h"

@interface LayoutSimple: Object {
}

+ layout: (LayoutOps *) ops;
+ attraction: (LayoutOps *) ops: (Node *) node: (Edge *) edge;
+ repulsion: (LayoutOps *) ops: (Node *) node1: (Node *) node2;
+ updateNode: (LayoutOps *) ops: (Node *) node;

@end

