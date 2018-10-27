#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

int main (void)
{
	NSAutoreleasePool *pool = [NSAutoreleasePool new];
	[NSApplication sharedApplication];
	NSRunAlertPanel (@"Test", @"Hello from GNUstep AppKit", nil, nil, nil);
	NSLog (@"Executing...");
	return 0;
}
