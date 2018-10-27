#import "edge.h"
#import "point.h"
#import <cairo.h>

@implementation Edge

- expose: (cairo_t *) context {
	cairo_set_source_rgb(context, 0.0, 0.0, 0.0);
	cairo_move_to(context, pointA->x, pointA->y);
	cairo_line_to(context, pointB->x, pointB->y);
	cairo_stroke(context);

	return self;
}

- (Point *) pointA {
	return pointA;
}

- (Point *) pointB {
	return pointB;
}

- pointA: (Point *) pa {
	pointA = pa;
	return self;
}

- pointB: (Point *) pb {
	pointB = pb;
	return self;
}

@end

