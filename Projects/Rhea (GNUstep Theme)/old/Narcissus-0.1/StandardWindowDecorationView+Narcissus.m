
#import <StandardWindowDecorationView+Narcissus.h>
#import <AppKit/AppKit.h>

#import "Drawing.h"

static NSDictionary* titleTextAttributes[3];

@implementation GSStandardWindowDecorationView (Narcissus)

+(void) initializeNarcissus {
	NSMutableParagraphStyle* p;
	
	p = [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
	[p setLineBreakMode: NSLineBreakByClipping];
	
	titleTextAttributes[0] = [[NSMutableDictionary alloc]
		initWithObjectsAndKeys:
			[NSFont titleBarFontOfSize: 0], NSFontAttributeName,
			[NSColor blackColor], NSForegroundColorAttributeName,
			p, NSParagraphStyleAttributeName,
		nil];
	
	titleTextAttributes[1] = [[NSMutableDictionary alloc]
		initWithObjectsAndKeys:
			[NSFont titleBarFontOfSize: 0], NSFontAttributeName,
			[NSColor darkGrayColor], NSForegroundColorAttributeName,
			p, NSParagraphStyleAttributeName,
		nil];
	
	titleTextAttributes[2] = [[NSMutableDictionary alloc]
		initWithObjectsAndKeys:
			[NSFont titleBarFontOfSize: 0], NSFontAttributeName,
			[NSColor darkGrayColor], NSForegroundColorAttributeName,
			p, NSParagraphStyleAttributeName,
		nil];
}

@end


@implementation GSStandardWindowDecorationView (theme)

-(void) drawTitleBar {
	NSString* title;
	static const float topGrays[3] = {0.8, 0.75, 0.75};
	
	title = [window title];
	
	// Fill background
	NCDrawGrayHorizontalGradient
	(titleBarRect, titleBarRect, topGrays[inputState], 0.68);
	
	// FIXME: use real window bg color here instead of 0.68!
	//[[bgColor colorUsingColorSpace: NSCalibratedBlackColorSpace] whiteComponent]);
	
	// Draw borders
	PSsetgray(0.8);
	PSmoveto(0, NSMaxY(titleBarRect) - 1.5);
	PSrlineto(titleBarRect.size.width, 0);
	PSstroke();
	
	// remove borders from buttons
	[miniaturizeButton setBordered: NO];
	[closeButton setBordered: NO];
	
	// Draw title string
	if (isTitled) {
		NSSize titleSize;
		NSRect workRect = titleBarRect;
		
		if (hasMiniaturizeButton) {
			workRect.origin.x += 17;
			workRect.size.width -= 17;
		}
		
		if (hasCloseButton) {
			workRect.size.width -= 17;
		}
		
		titleSize = [title sizeWithAttributes: titleTextAttributes[inputState]];
		if (titleSize.width <= workRect.size.width) {
			workRect.origin.x = NSMidX(workRect) - titleSize.width / 2;
		}
		
		workRect.origin.y = NSMidY(workRect) - titleSize.height / 2;
		workRect.size.height = titleSize.height;
		
		[title drawInRect: workRect
			withAttributes: titleTextAttributes[inputState]];
	}
	
	
}

-(void) drawResizeBar
{
	NSRect rbRect;
	NSRect borderRect;
	
	NSDivideRect(resizeBarRect, &borderRect, &rbRect, 2.0, NSMinYEdge);
	
	PSsetgray(0.4);
	NSRectFill(borderRect);
	
	NSDivideRect(resizeBarRect, &borderRect, &rbRect, 1.0, NSMaxYEdge);
	
	PSsetgray(0.5);
	NSRectFill(borderRect);
	
	NCDrawGrayHorizontalGradient(rbRect, rbRect, 0.8, 0.6);
}

@end
