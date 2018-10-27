#import "point.h"
#import <cairo.h>

@interface Node : Point {
@protected
	Point *velocity, *netforce;
	int size;
	BOOL locked;
}

- init;
- free;

- expose: (cairo_t *) context;
- (int) size;
- size: (int) val;
- (Point *) velocity;
- (Point *) netforce;
- lock: (BOOL) val;
- (BOOL) lock;

@end

