#import "node.h"
#import <cairo.h>
#import <math.h>

@implementation Node

- init {
	self = [super init];
	if (self) {
		velocity = [[Point alloc] init];
		netforce = [[Point alloc] init];
	}
	return self;
}

- free {
	[velocity free];
	[netforce free];
	return [super free];
}

- expose: (cairo_t *) context {
	if (locked)
		cairo_set_source_rgb(context, 1.0, 0.0, 0.0);
	else
		cairo_set_source_rgb(context, 0.0, 0.0, 0.0);

	cairo_arc(context, x, y, size / 6, 0, 2 * M_PI);
	cairo_stroke(context);

	return self;
}

- (int) size {
	return size;
}

- size: (int) val {
	size = val;
	return self;
}

- (Point *) velocity {
	return velocity;
}

- (Point *) netforce {
	return netforce;
}

- lock: (BOOL) val {
	locked = val;
	return self;
}

- (BOOL) lock {
	return locked;
}

@end

