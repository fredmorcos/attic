#import <Foundation/Foundation.h>
#import <xcb/xcb.h>

@interface XWindow: NSObject {
	xcb_window_t	window;
}

@end

