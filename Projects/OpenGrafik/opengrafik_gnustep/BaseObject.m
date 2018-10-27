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

#import "BaseObject.h"

@implementation BaseObject

- initWithFrame: (NSRect) frame {
	self = [super initWithFrame: frame];
	bgColor = [NSColor whiteColor];
	fgColor = [NSColor lightGrayColor];
	selected = NO;
	path = [NSBezierPath bezierPath];
	oldSize = NSMakeSize(0, 0);

	RETAIN(bgColor);
	RETAIN(fgColor);
	RETAIN(path);

	return self;
}

- (void) dealloc {
	TEST_RELEASE(name);
	RELEASE(bgColor);
	RELEASE(fgColor);
	RELEASE(path);
	[super dealloc];
}

- (void) setName: (NSString *) newName {
	ASSIGN(name, newName);
	[self changedProperty: @"Name"];
}

- (void) setBackgroundColor: (NSColor *) color {
	ASSIGN(bgColor, color);
	[self changedProperty: @"BackgroundColor"];
}

- (void) setForegroundColor: (NSColor *) color {
	ASSIGN(fgColor, color);
	[self changedProperty: @"ForegroundColor"];	
}

- (void) setSelected: (BOOL) sel {
	selected = sel;
	[self changedProperty: @"Selection"];
}

- (NSString *) name {
	return name;
}

- (NSColor *) backgroundColor {
	return bgColor;
}

- (NSColor *) foregroundColor {
	return fgColor;
}

- (BOOL) selected {
	return selected;
}

- (BOOL) sizeHasChanged {
	NSSize newSize = [self frame].size;
	BOOL	widthChanged = oldSize.width != newSize.width,
			heightChanged = oldSize.height != newSize.height;

	oldSize = newSize;

	if (widthChanged)
		return YES;
	else if (heightChanged)
		return YES;
	else
		return NO;
}

- (void) renderPath: (NSRect) rect {
}

- (void) drawRect: (NSRect) rect {
	[self renderPath: rect];
}

- (void) changedProperty: (NSString *) propName {
	NSString *className = [self className];
	NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
	NSString *notificationName = [NSString 
		stringWithFormat: @"%@%@", className, propName];

	[center postNotificationName: notificationName object: self];
	
	notificationName = [NSString stringWithFormat: @"%@Changed", className];
	[center postNotificationName: notificationName object: self];
}

@end

