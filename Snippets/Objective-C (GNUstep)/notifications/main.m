#import <Foundation/Foundation.h>
#import "Object1.h"
#import "Object2.h"

int main () {
	NSAutoreleasePool *pool = [NSAutoreleasePool new];
	Object1 *obj1 = [[Object1 alloc] init];
	Object2 *obj2 = [[Object2 alloc] init];
	[obj1 execClick];
	[obj2 listenForClick: obj1];
	[obj1 execClick];
	return 0;
}

