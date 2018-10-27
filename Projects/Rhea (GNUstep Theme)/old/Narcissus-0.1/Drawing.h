
#import <AppKit/AppKit.h>

/**
 * frame: the frame which is drawn
 * gradientRect: the frame of the gradient
 * topColor: the top color
 * bottomColor: the bottom color
 */
inline static void NCDrawHorizontalGradient
(NSRect frame, NSRect gradientRect, NSColor* topColor, NSColor* bottomColor)
{
	float ypos;
	int topR, topG, topB;
	int bottomR, bottomG, bottomB;
	
	topColor = [topColor colorUsingColorSpaceName: NSCalibratedRGBColorSpace];
	bottomColor = [bottomColor colorUsingColorSpaceName: NSCalibratedRGBColorSpace];

	topR = [topColor redComponent] * 255;
	topG = [topColor greenComponent] * 255;
	topB = [topColor blueComponent] * 255;
	bottomR = [bottomColor redComponent] * 255;
	bottomG = [bottomColor greenComponent] * 255;
	bottomB = [bottomColor blueComponent] * 255;
	
	
	for (ypos = NSMaxY(frame)-0.5; ypos >= NSMinY(frame); ypos -= 1.0) {
		int factor = (int)((255.0 * (NSMaxY(frame) - ypos)) / (frame.size.height));
		float currentR, currentG, currentB;
		currentR = (float)((bottomR*factor + topR*(255-factor))) / 65536.0;
		currentG = (float)((bottomG*factor + topG*(255-factor))) / 65536.0;
		currentB = (float)((bottomB*factor + topB*(255-factor))) / 65536.0;
		
		PSsetalpha(1.0);
		PSsetrgbcolor(currentR, currentG, currentB);
		PSmoveto(NSMinX(frame), ypos);
		PSrlineto(frame.size.width, 0);
		PSstroke();
	}
}


/**
 * frame: the frame which is drawn
 * gradientRect: the frame of the gradient
 * topGray: the top color
 * bottomGray: the bottom color
 */
inline static void NCDrawGrayHorizontalGradient
(NSRect frame, NSRect gradientRect, float topGray, float bottomGray)
{
	float ypos;
	int topG, bottomG;
	NSRect topRect;
	
	topG = topGray * 255;
	bottomG = bottomGray * 255;
	
	ypos = gradientRect.origin.y + 0.5;
	
	PSsetalpha(1.0);
	while(gradientRect.size.height >= 1.0) {
		int factor;
		float currentG;
		
		factor = (int)((255.0 * (NSMaxY(frame) - ypos)) / frame.size.height);
		currentG = (float)((bottomG*factor + topG*(255-factor))) / 65536.0;
		NSDivideRect(gradientRect, &topRect, &gradientRect, 1.0, NSMinYEdge);
		
		PSsetgray(currentG);
		NSRectFill(topRect);
		
		ypos += 1.0;
	}
}

/**
 * Draws a border with the given gray color.
 * Ignores alpha channel.
 * @return the rectangle /inside/ that border
 */
inline static NSRect NCDrawGrayBorder (
	NSRect frame,
	float borderGray
) {
	NSRect workRect;
	
	PSsetgray(borderGray);
	NSDivideRect(frame, &workRect, &frame, 1.0, NSMinYEdge); // top
	NSRectFill(workRect);
	
	NSDivideRect(frame, &workRect, &frame, 1.0, NSMaxYEdge); // bottom
	NSRectFill(workRect);
	
	NSDivideRect(frame, &workRect, &frame, 1.0, NSMinXEdge); // left
	NSRectFill(workRect);
	
	NSDivideRect(frame, &workRect, &frame, 1.0, NSMaxXEdge); // right
	NSRectFill(workRect);
	
	return frame;
}

/**
 * Draws the inside of a button.
 * Ignores alpha channel.
 */
inline static void NCFillGrayButton (
	NSRect frame,
	float topGray, float middleGray, float bottomGray
) {
	if (frame.size.height > 8) {
		NSRect topRect, middleRect, bottomRect;
		NSDivideRect(frame, &topRect, &middleRect, 3.0, NSMaxYEdge);
		NSDivideRect(middleRect, &bottomRect, &middleRect, 3.0, NSMinYEdge);
		
		NCDrawGrayHorizontalGradient(topRect, topRect, topGray, middleGray);
		PSsetgray(middleGray);
		NSRectFill(middleRect);
		NCDrawGrayHorizontalGradient(bottomRect, bottomRect, middleGray, bottomGray);
	} else {
		NCDrawGrayHorizontalGradient(frame, frame, topGray, bottomGray);
	}
}

inline static void NCDrawNSButton (
	NSRect frame,
	float borderGray,
	float highlightGray,
	float topGray,
	float middleGray,
	float bottomGray
) {
	NSRect workRect;
	
	// set opaque
	PSsetalpha(1.0);
	
	// draw hightlight
	PSsetgray(highlightGray);
	NSDivideRect(frame, &workRect, &frame, 1.0, NSMaxYEdge);
	NSRectFill(workRect);
	
	// draw border
	frame = NCDrawGrayBorder(frame, borderGray);
	
	// button fill
	NCFillGrayButton(frame, topGray, middleGray, bottomGray);
}

