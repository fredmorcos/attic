#import "gravity.h"

#import <cairo.h>
#import <math.h>

@implementation Gravity

- size: (int) val {
	size = -val;
	return self;
}

@end

