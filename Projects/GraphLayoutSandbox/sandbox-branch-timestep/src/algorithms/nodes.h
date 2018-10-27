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

