
#import "NSTableHeaderCell.h"

#import "Drawing.h"


@implementation NSTableHeaderCell (Narcissus)

-(void) drawWithFrame: (NSRect)cellFrame
	inView: (NSView*) controlView
{
	NSRect workRect;
	if (NSIsEmptyRect(cellFrame))
		return;
	
	[self setDrawsBackground: NO];
	
	//PSsetalpha(1.0);
	NSDivideRect(cellFrame, &workRect, &cellFrame, 1.0, NSMinXEdge);
	PSsetgray(0.7);
	NSRectFill(workRect);
	
	NSDivideRect(cellFrame, &workRect, &cellFrame, 1.0, NSMaxXEdge);
	PSsetgray(0.1);
	NSRectFill(workRect);
	
	//cellFrame = NCDrawGrayBorder(cellFrame, 0.1);
	
	if (_cell.is_highlighted == YES) {
		NCFillGrayButton(cellFrame, 0.7, 0.3, 0.1);
	} else {
		NCFillGrayButton(cellFrame, 0.2, 0.4, 0.7);
	}
	
	[self drawInteriorWithFrame: cellFrame inView: controlView];
}

@end

