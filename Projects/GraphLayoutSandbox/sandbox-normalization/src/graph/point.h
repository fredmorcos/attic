#import <objc/Object.h>

@interface Point : Object {
@protected
	double x, y;
}

- x: (double) val;
- y: (double) val;
- (double) x;
- (double) y;

@end

