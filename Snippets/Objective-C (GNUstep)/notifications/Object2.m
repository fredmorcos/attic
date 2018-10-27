#import "Object2.h"
#import "Object1.h"
#import <Foundation/Foundation.h>

@implementation Object2

- listenForClick: (Object1 *) obj1 {
	[[NSNotificationCenter defaultCenter] 
		addObserver: self 
		   selector: @selector(gotNotification)
			   name: @"clicked"
			 object: nil];
	return self;
}

- gotNotification {
	NSLog(@"Got notification from Object 1!!!");
	return self;
}

@end

