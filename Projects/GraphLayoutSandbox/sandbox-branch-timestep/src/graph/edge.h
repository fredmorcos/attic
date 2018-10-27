#import <objc/Object.h>
#import "point.h"
#import <cairo.h>

@interface Edge : Object {
@protected
	Point *pointA, *pointB;
}

- expose: (cairo_t *) context;
- (Point *) pointA;
- (Point *) pointB;
- pointA: (Point *) pa;
- pointB: (Point *) pb;

@end

