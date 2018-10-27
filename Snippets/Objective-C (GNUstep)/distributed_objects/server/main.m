#import <Foundation/Foundation.h>
#import "Reader.h"

int main () {
	NSAutoreleasePool *pool;
	Reader *reader;
	NSConnection *conn;
	BOOL registered;

	pool = [NSAutoreleasePool new];
	reader = [Reader new];
	conn = [NSConnection defaultConnection];

	[conn setRootObject: reader];
	registered = [conn registerName: @"FileReader"];

	if (!registered) {
		NSLog(@"Couldn't register service.");
		exit(1);
	}

	NSLog(@"Server registered...");
	NSLog(@"Waiting for connections...");

	[[NSRunLoop currentRunLoop] run];
	return 0;
}

