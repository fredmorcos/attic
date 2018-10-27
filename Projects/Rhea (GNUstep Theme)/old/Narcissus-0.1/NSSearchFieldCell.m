
#import <AppKit/AppKit.h>
#import "NSSearchFieldCell.h"

@implementation NSSearchFieldCell (Narcissus)

- (void) drawWithFrame: (NSRect)cellFrame inView: (NSView*)controlView
{
	if (NSIsEmptyRect(cellFrame)) {
		return;
	}

	[[NSColor redColor] set];
	NSRectFill(cellFrame);
	
	[_search_button_cell drawWithFrame: [self searchButtonRectForBounds: cellFrame] inView: controlView];
	[super drawWithFrame: [self searchTextRectForBounds: cellFrame] inView: controlView];
	[_cancel_button_cell drawWithFrame: [self cancelButtonRectForBounds: cellFrame] inView: controlView];
}

@end

