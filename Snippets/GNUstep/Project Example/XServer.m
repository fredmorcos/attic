#import "XServer.h"

@implementation XServer

- init {
	self = [super init];
	connection = [XConnection new];
	return self;
}

- (void) dealloc {
	[connection dealloc];
	[super dealloc];
}

@end

