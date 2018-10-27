#import <Foundation/Foundation.h>
#import "AppDelegate.h"

@implementation AppDelegate: NSObject

- (void) dealloc
{
	RELEASE (window);
	[super dealloc];
}

- (void) createMenu
{
	NSMenu *mainMenu = AUTORELEASE([NSMenu new]);
	NSMenu *infoMenu = AUTORELEASE([NSMenu new]);
	NSMenuItem *infoItem;

	[mainMenu addItemWithTitle: @"Quit" 
						action: @selector (terminate:)
				 keyEquivalent: @"q"];

	[infoMenu addItemWithTitle: @"About"
						action: @selector (orderFrontStandardInfoPanel:)
				 keyEquivalent: @""];

	infoItem = [mainMenu addItemWithTitle: @"Help"
								   action: NULL
							keyEquivalent: @""];
	[mainMenu setSubmenu: infoMenu forItem: infoItem];

	[NSApp setMainMenu: mainMenu];
}

- (void) createWindow
{
	NSButton *button;
	NSSize buttonSize;
	NSRect rect;
	unsigned int windowMask;

	button = AUTORELEASE([NSButton new]);
	[button setTitle: @"Hello"];
	[button sizeToFit];
	[button setTarget: self];
	[button setAction: @selector (printHello:)];
	buttonSize = [button frame].size;

	rect = NSMakeRect (100, 100, 
			buttonSize.width,
			buttonSize.height);

	windowMask = 
		NSTitledWindowMask 
		| NSMiniaturizableWindowMask
		| NSResizableWindowMask;

	window = [NSWindow alloc];
	[window initWithContentRect: rect 
					  styleMask: windowMask
						backing: NSBackingStoreBuffered
						  defer: NO];
	[window setTitle: @"sandbox"];
	[window setContentView: button];
}

- (void) applicationWillFinishLaunching: (NSNotification *) not
{
	[self createMenu];
	[self createWindow];
}

- (void) applicationDidFinishLaunching: (NSNotification *) not
{
	[window makeKeyAndOrderFront: nil];
}

- (void) printHello: (id) sender
{
	NSLog(@"Hello!");
}

@end

