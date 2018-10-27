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

- init {
	self = [super init];
	bgColor = [NSColor whiteColor];
	fgColor = [NSColor lightGrayColor];
	path = [NSBezierPath bezierPath];
	rect = NSMakeRect(0, 0, 100, 100);
	rotation = 0;
	selected = NO;
	parent = nil;
	return self;
}

- (void) dealloc {
	TEST_RELEASE(name);
	RELEASE(bgColor);
	RELEASE(fgColor);
	RELEASE(path);
	TEST_RELEASE(parent);
	[super dealloc];
}

- setName: (NSString *) newName {
	ASSIGN(name, newName);
	[self changedProperty: @"Name"];
	return self;
}

- setBackgroundColor: (NSColor *) color {
	ASSIGN(bgColor, color);
	[self changedProperty: @"BackgroundColor"];
	return self;
}

- setForegroundColor: (NSColor *) color {
	ASSIGN(fgColor, color);
	[self changedProperty: @"ForegroundColor"];	
	return self;
}

- setSize: (NSSize) size {
	rect.size = size;
	[self changedProperty: @"Size"];
	return self;
}

- setOrigin: (NSPoint) origin {
	rect.origin = origin;
	[self changedProperty: @"Origin"];
	return self;
}

- setRotation: (int) rot {
	rotation = rot;
	[self changedProperty: @"Rotation"];
	return self;
}

- setSelected: (BOOL) sel {
	selected = sel;
	[self changedProperty: @"Selection"];
	return self;
}

- setVisible: (BOOL) vis {
	visible = vis;
	[self changedProperty: @"Visibility"];
	return self;
}

- setParent: (id) par {
	ASSIGN(parent, par);
	return self;
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

- (NSSize) size {
	return rect.size;
}

- (NSPoint) origin {
	return rect.origin;
}

- (int) rotation {
	return rotation;
}

- (BOOL) selected {
	return selected;
}

- (BOOL) visible {
	return visible;
}

- (id) parent {
	return parent;
}

- render {
	return self;
}

- stroke {
	return self;
}

- changedProperty: (NSString *) propName {
	NSString *className = [self className];
	NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
	NSString *notificationName = [NSString 
		stringWithFormat: @"%@%@", className, propName];

	[center postNotificationName: notificationName object: self];
	RELEASE(notificationName);
	
	notificationName = [NSString stringWithFormat: @"%@Changed", className];
	[center postNotificationName: notificationName object: self];
	RELEASE(notificationName);
	
	return self;
}

@end

