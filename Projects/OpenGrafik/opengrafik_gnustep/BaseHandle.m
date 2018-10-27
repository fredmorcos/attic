/*	
 *	This file is part of OpenGrafik.
 *
 *	Copyright 2009	Michael Morckos <mikey.morckos@gmail.com>
 *					Frederic Morcos <fred.morcos@gmail.com>
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
 
#import "ConnectionHandle.h"
 
@implementation BaseHandle

- initWithFrame: (NSRect) frame {
	self = [super initWithFrame: frame];
	[self limitFrame];
	[self initColors];
	[self renderPath: [self bounds]];
	return self;
}

- init {
	self = [super init];
	[self limitFrame];
	[self initColors];
	return self;
}

- limitFrame {
	NSRect frame = [self frame];
	frame.size.width = 16;
	frame.size.height = 16;
	[self setFrame: frame];
	return self;
}

- initColors {
	[self setBackgroundColor: [NSColor colorWithCalibratedRed: 0.1 
					   green: 0.7 blue: 0.1 alpha: 0.8]];
	[self setForegroundColor: [NSColor colorWithCalibratedRed: 0.1
					   green: 0.5 blue: 0.1 alpha: 1.0]];
	return self;
}

- setOwner: (BaseObject *) newOwner {
	ASSIGN(owner, newOwner);
	return self;
}

- (BaseObject *) owner {
	return owner;
}

- (void) dealloc {
	TEST_RELEASE(owner);
	[super dealloc];	
}

- (void) renderPath: (NSRect) rect {
	int lw = 1;
	
	[path setLineWidth: 1];
	
	rect.origin.x += lw;
	rect.origin.y += lw;
	rect.size.width -= lw * 2;
	rect.size.height -= lw * 2;
	
	[path appendBezierPathWithRect: rect];
}

- (void) drawRect: (NSRect) rect {
	[bgColor set];
	[path fill];
	[fgColor set];
	[path stroke];
}

@end

