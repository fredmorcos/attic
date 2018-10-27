
#import "NSMenuItemCell.h"

#import "Drawing.h"

#import <AppKit/NSMenuView.h>

@implementation NSMenuItemCell (Narcissus)

- (void) drawBorderAndBackgroundWithFrame: (NSRect)cellFrame
                                  inView: (NSView *)controlView
{
	unsigned  mask;
	float topG, middleG, bottomG;
	
	// Fall back to standard behavior for horizontal menus
	if ([_menuView isHorizontal] == YES) {
		cellFrame = [self drawingRectForBounds: cellFrame];
		[[self backgroundColor] set];
		NSRectFill(cellFrame);
		return;
	}
	
	// find out the highlighting state (stolen from -backgroundColor method)
	if (_cell.is_highlighted) {
		mask = _highlightsByMask;
		
		if (_cell.state) {
			mask &= ~_showAltStateMask;
		}
	} else if (_cell.state) {
		mask = _showAltStateMask;
	} else {
		mask = NSNoCellMask;
	}
	
	// Determine the background color
	if (mask & (NSChangeGrayCellMask | NSChangeBackgroundCellMask)) {
		topG = 1.0;
		middleG = 0.8;
		bottomG = 0.9;
	} else {
		topG = 0.9;
		middleG = 0.7;
		bottomG = 0.5;
	}
	
	// flip if necessary
	if ([controlView isFlipped]) {
		float tmp = topG;
		topG = bottomG;
		bottomG = tmp;
	}
	
	// draw
	cellFrame = NCDrawGrayBorder(cellFrame, 0.5);
	NCFillGrayButton(cellFrame, topG, middleG, bottomG);
}


@end

