#import <Foundation/Foundation.h>
#import <xcb/xcb.h>
#import "XScreen.h"

@interface XConnection: NSObject {
	xcb_connection_t	*connection;
}

- initWithDisplayName: (const char *) displayName;
- (void) dealloc;

@end

