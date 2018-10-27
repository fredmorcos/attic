#import "Object1.h"
#import <Foundation/Foundation.h>

@implementation Object1

- execClick {
	[[NSNotificationCenter defaultCenter] postNotificationName: @"clicked" object: nil];
	return self;
}

@end

