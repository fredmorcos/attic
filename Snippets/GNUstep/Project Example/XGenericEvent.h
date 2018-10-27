#import <Foundation/Foundation.h>
#import <xcb/xcb.h>

@interface XGenericEvent: NSObject {
	xcb_generic_event_t		*event;
}

@end

