#import <Foundation/Foundation.h>
#import <xcb/xcb.h>

@interface XScreen: NSObject {
	xcb_screen_t	*screen;
}

@end

