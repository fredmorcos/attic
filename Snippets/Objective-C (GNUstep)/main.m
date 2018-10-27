#import "AppDelegate.h"

int main (int argc, const char **argv)
{
	NSAutoreleasePool *pool = [NSAutoreleasePool new];
	[NSApplication sharedApplication];
	[NSApp setDelegate: [AppDelegate new]];

	return NSApplicationMain (argc, argv);
}

