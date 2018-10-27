#import <objc/Object.h>
#import <graph.h>
#import <node.h>

@interface Layout: Object {
}

+ (Point *) forceLayout: (double) c: (double) k: (double) t: (double) d: (Graph *) g;

+ (Point *) hookeatt: (double) k: (Node *) n: (Edge *) e;
// + (double) hooke_scalar: (double) k: (Edge *) e;
+ (Point *) coulrep: (double) c: (Node *) n1: (Node *) n2;
// + (double) coul_scalar: (double) c: (Node *) n1: (Node *) n2;
// + (double) theta: (Point *) cur: (Point *) origin;
+ (int) distance: (Point *) n1: (Point *) n2;

@end

