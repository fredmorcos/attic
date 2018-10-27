
#import "GSToolbar.h"

#import "Drawing.h"

@implementation GSToolbarView (Themeability)

- (BOOL) isOpaque
{
	return NO;
}


- (void) drawRect: (NSRect)aRect
{
  [_clipView setDrawsBackground: NO];
  
  //[super drawRect: aRect];
  
  NSRect viewFrame = [self frame];
  NCDrawGrayHorizontalGradient(aRect, aRect, 0.68, 0.5);
  
  
  // We draw the border
  PSsetgray(0.4);
  
  if (_borderMask & GSToolbarViewBottomBorder)
  {
    [NSBezierPath strokeLineFromPoint: NSMakePoint(0, 0.5) 
                              toPoint: NSMakePoint(viewFrame.size.width, 0.5)];
  }
  if (_borderMask & GSToolbarViewTopBorder)
  {
    [NSBezierPath strokeLineFromPoint: NSMakePoint(0, 
                                         viewFrame.size.height - 0.5) 
                              toPoint: NSMakePoint(viewFrame.size.width, 
                                         viewFrame.size.height -  0.5)];
  }
  if (_borderMask & GSToolbarViewLeftBorder)
  {
    [NSBezierPath strokeLineFromPoint: NSMakePoint(0.5, 0) 
                              toPoint: NSMakePoint(0.5, viewFrame.size.height)];
  }
  if (_borderMask & GSToolbarViewRightBorder)
  {
    [NSBezierPath strokeLineFromPoint: NSMakePoint(viewFrame.size.width - 0.5,0)
                              toPoint: NSMakePoint(viewFrame.size.width - 0.5, 
                                         viewFrame.size.height)];
  }
  
}

@end
