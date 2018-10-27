
#import "NarcissusTheme.h"
#import "StandardWindowDecorationView+Narcissus.h"
#import "Drawing.h"
#import "NSScroller.h"

@implementation NarcissusTheme

// ---------------------------------------------
//    Theme activation and initialisation
// ---------------------------------------------

+(void)load {
	if (self == NSClassFromString(@"NarcissusTheme")) {
		NSLog(@"Loading Narcissus theme");
		[GSStandardWindowDecorationView initializeNarcissus];
		[GSTheme setTheme: [self new]];
	}
}

/*
 * Called when the theme is activated
 */
-(void) activate {
	[super activate];
}

/*
 * Called when the theme is activated
 */
-(void) deactivate {
	[super deactivate];
}

// ---------------------------------------------
//    General info about a theme
// ---------------------------------------------
-(NSImage*) icon {
	return [NSImage imageNamed: @"GNUstep"];
}

// ---------------------------------------------
//    Actual drawing
// ---------------------------------------------

/*
-(NSRect) drawButton: (NSRect) border
	withClip: (NSRect) clip
{
	NSRect workRect = border;
	
	PSsetgray(0.5);
	PSmoveto(NSMinX(border), NSMinY(border) + 0.5);
	PSrlineto(border.size.width, 0);
	PSstroke();
	
	workRect.size.height -= 1.0;
	workRect.origin.y += 1.0;
	[super drawButton: workRect withClip: clip];
	return border;
}
*/
-(NSRect) drawButton: (NSRect) border
	withClip: (NSRect) clip
{
	PSsetalpha(1.0);
	NCFillGrayButton(border, 0.8, 0.6, 0.4);
	return border;
}

-(NSRect) drawGrayBezel: (NSRect) border
	withClip: (NSRect) clip
{
	PSsetalpha(1.0);
	border = NCDrawGrayBorder(border, 0.6);
	return border;
}

-(NSRect) drawDarkButton: (NSRect) border
	withClip: (NSRect) clip
{
	PSsetalpha(1.0);
	border = NCDrawGrayBorder(border, 0.6);
	NCFillGrayButton(border, 0.3, 0.6, 0.3);
	return border;
}

- (void) drawButton: (NSRect)frame 
                 in: (NSButtonCell*)cell 
               view: (NSView*)view 
              style: (int)style 
              state: (GSThemeControlState)state
{
	float topG, middleG, bottomG;
	NSColor* color = nil;
	
	if (state == GSThemeNormalState) {
		topG = 0.9;
		middleG = 0.8;
		bottomG = 0.9;
		color = [NSColor controlBackgroundColor];
	} else if (state == GSThemeHighlightedState) {
		topG = 0.5;
		middleG = 0.4;
		bottomG = 0.3;
		color = [NSColor selectedControlColor];
	} else if (state == GSThemeSelectedState) {
		topG = 0.5;
		middleG = 0.3;
		bottomG = 0.2;
		color = [NSColor selectedControlColor];
	}
	
	switch (style) {
	case NSRoundRectBezelStyle:
	case NSTexturedRoundBezelStyle:
	case NSRoundedBezelStyle:
		[self drawRoundBezel: frame withColor: color];
		break;
	case NSTexturedSquareBezelStyle:
		frame = NSInsetRect(frame, 0, 1);
	case NSSmallSquareBezelStyle:
	case NSRegularSquareBezelStyle:
	case NSShadowlessSquareBezelStyle:
		NCDrawGrayHorizontalGradient(frame, frame, topG, bottomG);
		//[color set];
		//NSRectFill(frame);
		[[NSColor controlShadowColor] set];
		NSFrameRectWithWidth(frame, 1);
		break;
	case NSThickSquareBezelStyle:
		NCDrawGrayHorizontalGradient(frame, frame, topG, bottomG);
		//[color set];
		//NSRectFill(frame);
		[[NSColor controlShadowColor] set];
		NSFrameRectWithWidth(frame, 1.5);
		break;
	case NSThickerSquareBezelStyle:
		//[color set];
		//NSRectFill(frame);
		NCDrawGrayHorizontalGradient(frame, frame, topG, bottomG);
		[[NSColor controlShadowColor] set];
		NSFrameRectWithWidth(frame, 2);
		break;
	case NSCircularBezelStyle:
		frame = NSInsetRect(frame, 3, 3);
	case NSHelpButtonBezelStyle:
		[self drawCircularBezel: frame withColor: color]; 
		break;
	case NSDisclosureBezelStyle:
	case NSRoundedDisclosureBezelStyle:
	case NSRecessedBezelStyle:
		// FIXME
		break;
	default:
		NCDrawNSButton(frame, 0.5, 0.6, topG, middleG, bottomG);
	}
}


/*
-(void) drawButton: (NSRect)frame
	in: (NSButtonCell*)cell
	view: (NSView*)view
	style: (int)style
	state: (GSThemeControlState)state {
	
	PSmoveto(NSMinX(frame)+0.5
	//if (frame.size.height > 30.0) {
	NCDrawGrayHorizontalGradient(frame, frame, 1.0, 0.9);
	
	
	[super drawButton: frame
		in: cell
		view: view
		style: style
		state: state];
}
*/
@end

@implementation NarcissusTheme (Factory)

- (NSButtonCell*) makeScrollerKnobCell {
  NSButtonCell* knobCell = [NarcissusScrollerButtonCell new];
  [knobCell setButtonType: NSMomentaryChangeButton];
  [knobCell setImage: [NSImage imageNamed: @"common_Dimple"]];
  [knobCell setImagePosition: NSImageOnly];
  return knobCell;
}


- (NSButtonCell*) makeScrollerUpCell {
  NSButtonCell* upCell = [NarcissusScrollerButtonCell new];
  [upCell setHighlightsBy: NSChangeBackgroundCellMask | NSContentsCellMask];
  [upCell setImage: [NSImage imageNamed: @"common_ArrowUp"]];
  [upCell setAlternateImage: [NSImage imageNamed: @"common_ArrowUpH"]];
  [upCell setImagePosition: NSImageOnly];
  [upCell setContinuous: YES];
  [upCell sendActionOn: (NSLeftMouseDownMask | NSPeriodicMask)];
  [upCell setPeriodicDelay: 0.3 interval: 0.03];
  return upCell;
}

- (NSButtonCell*) makeScrollerDownCell {
  NSButtonCell* downCell = [NarcissusScrollerButtonCell new];
  [downCell setHighlightsBy: NSChangeBackgroundCellMask | NSContentsCellMask];
  [downCell setImage: [NSImage imageNamed: @"common_ArrowDown"]];
  [downCell setAlternateImage: [NSImage imageNamed: @"common_ArrowDownH"]];
  [downCell setImagePosition: NSImageOnly];
  [downCell setContinuous: YES];
  [downCell sendActionOn: (NSLeftMouseDownMask | NSPeriodicMask)];
  [downCell setPeriodicDelay: 0.3 interval: 0.03];
  return downCell;
}

- (NSButtonCell*) makeScrollerLeftCell {
  NSButtonCell* leftCell = [NarcissusScrollerButtonCell new];
  [leftCell setHighlightsBy: NSChangeBackgroundCellMask | NSContentsCellMask];
  [leftCell setImage: [NSImage imageNamed: @"common_ArrowLeft"]];
  [leftCell setAlternateImage: [NSImage imageNamed: @"common_ArrowLeftH"]];
  [leftCell setImagePosition: NSImageOnly];
  [leftCell setContinuous: YES];
  [leftCell sendActionOn: (NSLeftMouseDownMask | NSPeriodicMask)];
  [leftCell setPeriodicDelay: 0.3 interval: 0.03];
  return leftCell;
}

- (NSButtonCell*) makeScrollerRightCell {
  NSButtonCell* rightCell = [NarcissusScrollerButtonCell new];
  [rightCell setHighlightsBy: NSChangeBackgroundCellMask | NSContentsCellMask];
  [rightCell setImage: [NSImage imageNamed: @"common_ArrowRight"]];
  [rightCell setAlternateImage: [NSImage imageNamed: @"common_ArrowRightH"]];
  [rightCell setImagePosition: NSImageOnly];
  [rightCell setContinuous: YES];
  [rightCell sendActionOn: (NSLeftMouseDownMask | NSPeriodicMask)];
  [rightCell setPeriodicDelay: 0.3 interval: 0.03];
  return rightCell;
}

@end


