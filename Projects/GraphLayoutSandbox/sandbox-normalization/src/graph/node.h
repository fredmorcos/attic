#import "point.h"
#import <cairo.h>

@interface Node : Point {
@protected
	Point *netforce, *velocity;
	int size, mass;
	BOOL locked;
}

- init;
- free;

- expose: (cairo_t *) context;
- (int) size;
- size: (int) val;
- (int) mass;
- mass: (int) val;
- netforce: (Point *) val;
- (Point *) netforce;
- velocity: (Point *) val;
- (Point *) velocity;
- lock: (BOOL) val;
- (BOOL) lock;

@end

