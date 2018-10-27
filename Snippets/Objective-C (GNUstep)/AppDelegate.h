#import <AppKit/AppKit.h>

@interface AppDelegate: NSObject {
	NSWindow *window;
}

- (void) createMenu;
- (void) createWindow;

- (void) applicationWillFinishLaunching: (NSNotification *) not;
- (void) applicationDidFinishLaunching: (NSNotification *) not;

- (void) printHello: (id) sender;

@end
