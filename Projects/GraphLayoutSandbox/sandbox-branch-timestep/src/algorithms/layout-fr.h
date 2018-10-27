#import <objc/Object.h>
#import <graph.h>

#import "layout.h"

@interface LayoutFR: Object {
}

+ layout: (LayoutOps *) ops;
+ attraction: (LayoutOps *) ops: (Edge *) edge;
+ repulsion: (LayoutOps *) ops: (Node *) node1: (Node *) node2;
+ updateNode: (LayoutOps *) ops: (Node *) node;

@end

