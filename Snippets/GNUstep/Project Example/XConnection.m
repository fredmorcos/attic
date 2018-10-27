#import "XConnection.h"

@implementation XConnection

- init {
	self = [super init];
	connection = xcb_connect(NULL, NULL);
	return self;
}

- (void) dealloc {
	xcb_disconnect(connection);
	[super dealloc];
}

@end

