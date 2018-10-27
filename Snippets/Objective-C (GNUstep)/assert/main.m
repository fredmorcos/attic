#import <Foundation/Foundation.h>

int main () {
	NSAutoreleasePool *pool = [NSAutoreleasePool new];
	NSCAssert(1 == 2, @"One not equal to Two!");
	return 0;
}

