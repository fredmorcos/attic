#import <Foundation/Foundation.h>
#import "XConnection.h"

@interface XServer: NSObject {
	XConnection		*connection;
}

- init;
- (void) dealloc;

@end

