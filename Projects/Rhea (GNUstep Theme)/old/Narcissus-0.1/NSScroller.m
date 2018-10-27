
#import "NSScroller.h"
#import "Drawing.h"

// floor()
#include <math.h>

@implementation NSScroller (Narcissus)


static const float buttonsOffset = 2;

- (void) drawKnobSlot
{
	static NSRect rect;
	
	if (_cacheValid == NO) {
		rect = [self rectForPart: NSScrollerKnobSlot];
	}
	
	rect = NCDrawGrayBorder(rect, 0.5);
	NCFillGrayButton(rect, 0.5, 0.4, 0.2);
}

#if 0
/**
 */
- (NSRect) rectForPart: (NSScrollerPart)partCode
{
  NSRect scrollerFrame = _frame;
  float x = 1, y = 1;
  float width, height;
  float buttonsWidth = ([isa scrollerWidth] - buttonsOffset);
  float buttonsSize = 2 * buttonsWidth + 2;
  NSUsableScrollerParts usableParts;
  /*
   * If the scroller is disabled then the scroller buttons and the
   * knob are not displayed at all.
   */
  if (!_isEnabled)
    {
      usableParts = NSNoScrollerParts;
    }
  else
    {
      usableParts = _usableParts;
    }

  /*
   * Assign to `width' and `height' values describing
   * the width and height of the scroller regardless
   * of its orientation.
   * but keeps track of the scroller's orientation.
   */
  if (_isHorizontal)
    {
      width = scrollerFrame.size.height;// - 2;
      height = scrollerFrame.size.width;// - 2;
    }
  else
    {
      width = scrollerFrame.size.width;// - 2;
      height = scrollerFrame.size.height;// - 2;
    }

  /*
   * The x, y, width and height values are computed below for the vertical
   * scroller.  The height of the scroll buttons is assumed to be equal to
   * the width.
   */
  switch (partCode)
    {
      case NSScrollerKnob:
	{
	  float knobHeight, knobPosition, slotHeight;

	  if (usableParts == NSNoScrollerParts
	    || usableParts == NSOnlyScrollerArrows)
	    {
	      return NSZeroRect;
	    }

	  /* calc the slot Height */
	  slotHeight = height - (_arrowsPosition == NSScrollerArrowsNone
				 ?  0 : buttonsSize);
	  knobHeight = _knobProportion * slotHeight;
	  knobHeight = (float)floor(knobHeight);
	  if (knobHeight < buttonsWidth)
	    knobHeight = buttonsWidth;

	  /* calc knob's position */
	  knobPosition = _floatValue * (slotHeight - knobHeight);
	  knobPosition = floor(knobPosition);


	  /* calc actual position */
	  y += knobPosition + ((_arrowsPosition == NSScrollerArrowsMaxEnd
			       || _arrowsPosition == NSScrollerArrowsNone)
			       ?  0 : buttonsSize);
	  height = knobHeight;
	  width = buttonsWidth;
	  break;
	}

      case NSScrollerKnobSlot:
	/*
	 * if the scroller does not have buttons the slot completely
	 * fills the scroller.
	 */
	if (usableParts == NSNoScrollerParts
	  || _arrowsPosition == NSScrollerArrowsNone)
	  {
	    break;
	  }
	height -= buttonsSize;
	if (_arrowsPosition == NSScrollerArrowsMinEnd)
	  {
	    y += buttonsSize;
	  }
	break;

      case NSScrollerDecrementLine:
      case NSScrollerDecrementPage:
	if (usableParts == NSNoScrollerParts
	  || _arrowsPosition == NSScrollerArrowsNone)
	  {
	    return NSZeroRect;
	  }
	else if (_arrowsPosition == NSScrollerArrowsMaxEnd)
	  {
	    y += (height - buttonsSize + 1);
	  }
	width = buttonsWidth;
	height = buttonsWidth;
	break;

      case NSScrollerIncrementLine:
      case NSScrollerIncrementPage:
	if (usableParts == NSNoScrollerParts
	  || _arrowsPosition == NSScrollerArrowsNone)
	  {
	    return NSZeroRect;
	  }
	else if (_arrowsPosition == NSScrollerArrowsMaxEnd)
	  {
	    y += (height - buttonsWidth);
	  }
	else if (_arrowsPosition == NSScrollerArrowsMinEnd)
	  {
	    y += (buttonsWidth + 1);
	  }
	height = buttonsWidth;
	width = buttonsWidth;
	break;

      case NSScrollerNoPart:
	return NSZeroRect;
    }

  if (_isHorizontal)
    {
      return NSMakeRect (y, x, height, width);
    }
  else
    {
      return NSMakeRect (x, y, width, height);
    }
}
#endif

@end

@implementation NarcissusScrollerButtonCell

-(void) _drawBorderAndBackgroundWithFrame: (NSRect) cellFrame
	inView: (NSView*) controlView {
	cellFrame = NCDrawGrayBorder(cellFrame, 0.5);
	NCFillGrayButton(cellFrame, 0.8, 0.7, 0.8);
}

@end


