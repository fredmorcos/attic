/*	
 *	This file is part of OpenGrafik.
 *
 *	Copyright 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	OpenGrafik is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	OpenGrafik is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OpenGrafik.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "GridView.h"

@implementation GridView

- initWithFrame: (NSRect) frame {
	self = [super initWithFrame: frame];
	properties = [GridProperties sharedGridProperties];
	[self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
	[[NSNotificationCenter defaultCenter] 
		addObserver: self selector: @selector(redisplay) 
		name: @"OGGridVisualPropertyChanged" object: properties];
	return self;
}

- (void) drawRect: (NSRect) rect {
	int spacing = [properties spacing];
	BOOL visible = [properties visible];
	
	// rect = [self frame];
	
	[NSBezierPath clipRect: rect];
	[[NSColor whiteColor] set];
	[NSBezierPath fillRect: rect];

	if (visible == YES) {
		// FIXME segfaults
		// [[properties color] set];
		[[NSColor lightGrayColor] set];
		
		for (int i = spacing; i < rect.size.width; i += spacing)
			[NSBezierPath strokeLineFromPoint: NSMakePoint(i, 0)
				toPoint: NSMakePoint(i, rect.size.height)];
		
		for (int i = spacing; i < rect.size.height; i += spacing)
			[NSBezierPath strokeLineFromPoint: NSMakePoint(0, i)
				toPoint: NSMakePoint(rect.size.width, i)];
	}
}

- redisplay {
	[self setNeedsDisplay: YES];
	return self;
}

@end

