#import "node.h"
#import <cairo.h>
#import <math.h>

@implementation Node

- init {
	self = [super init];
	if (self) {
		netforce = [[Point alloc] init];
		velocity = [[Point alloc] init];
	}
	return self;
}

- free {
	[netforce free];
	[velocity free];
	return [super free];
}

- expose: (cairo_t *) context {
	if (locked)
		cairo_set_source_rgba(context, 1.0, 0.0, 0.0, 0.5);
	else
		cairo_set_source_rgba(context, 0.0, 0.0, 0.0, 0.5);

//	cairo_rectangle(context, x - (width / 2), y - (height / 2), width, height);
	cairo_arc(context, x, y, size / 2, 0, 2 * M_PI);
	cairo_fill(context);

	return self;
}

- (int) size {
	return size;
}

- size: (int) val {
	size = val;
	return self;
}

- (int) mass {
	return mass;
}

- mass: (int) val {
	mass = val;
	return self;
}

- netforce: (Point *) val {
	netforce = val;
	return self;
}

- (Point *) netforce {
	return netforce;
}

- velocity: (Point *) val {
	velocity = val;
	return self;
}

- (Point *) velocity {
	return velocity;
}

- lock: (BOOL) val {
	locked = val;
	return self;
}

- (BOOL) lock {
	return locked;
}

@end

