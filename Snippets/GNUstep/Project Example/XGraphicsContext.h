#import <Foundation/Foundation.h>
#import <xcb/xcb.h>

@interface XGraphicsContext: NSObject {
	xcb_gcontext_t		gcontext;
}

@end

