#import "NSStack.h"

@implementation NSStack

- pushObject: (id) object {
	[self insertObject: object atIndex: 0];
	return self;
}

- popObject {
	id temp = [self objectAtIndex: 0];
	[self removeObjectAtIndex: 0];
	return temp;
}

- topObject {
	return [self objectAtIndex: 0];
}

@end

