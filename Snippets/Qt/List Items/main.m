#import <Foundation/Foundation.h>

int main () {
	NSAutoreleasePool *pool = [NSAutoreleasePool new];
//	NSDirectoryEnumerator *de = [[NSFileManager defaultManager] enumeratorAtPath: NSHomeDirectory()];
//	NSDirectoryEnumerator *de = [[NSFileManager defaultManager] enumeratorAtPath: @"/personal/workspace/examples/"];
//	id tmp;
//	while ((tmp = [de nextObject]))
//		NSLog(tmp);

	NSArray *arr = [[NSFileManager defaultManager] componentsToDisplayForPath: NSHomeDirectory()];
	NSEnumerator *e = [arr objectEnumerator];
	id tmp;
	while ((tmp = [e nextObject])) 
		NSLog(tmp);
	return 0;
}

