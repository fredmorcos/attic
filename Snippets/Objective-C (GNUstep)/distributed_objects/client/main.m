#import <Foundation/Foundation.h>

@protocol Reader
- (NSString *) getFile: (NSString *) fileName;
@end

int main () {
	NSAutoreleasePool *pool;
	NSArray *args;
	id <Reader> reader;
	NSString *filename;
	NSString *file;

	pool = [NSAutoreleasePool new];
	reader = (id <Reader>) [NSConnection 
		rootProxyForConnectionWithRegisteredName: @"FileReader" 
											host: @""];

	if (reader == nil) {
		NSLog(@"Couldn't get object from network.");
		exit(1);
	}

	if (![reader respondsToSelector: @selector(getFile:)]) {
		NSLog(@"Found object on network but it won't respond to message.");
		exit(1);
	}

	args = [[NSProcessInfo processInfo] arguments];
	if ([args count] <= 1) {
		NSLog(@"No filename given.");
		exit(1);
	}

	filename = [args objectAtIndex: 1];
	file = [reader getFile: filename];

	if (file == nil) {
		NSLog(@"Couldn't read the file.");
		exit(1);
	}

	printf("%s\n", [file lossyCString]);
	return 0;
}

