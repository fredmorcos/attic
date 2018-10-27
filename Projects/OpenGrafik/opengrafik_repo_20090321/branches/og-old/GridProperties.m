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

#import "GridProperties.h"

GridProperties *sharedGridProperties;

@implementation GridProperties

+ sharedGridProperties {
	if (!sharedGridProperties)
		sharedGridProperties = [GridProperties new];
	return sharedGridProperties;
}

- init {
	self = [super init];
	[self setColor: [NSColor lightGrayColor]];
	[self setSpacing: 20];
	[self setSnapDistance: 5];
	[self setSnap: NO];
	[self setVisible: YES];
	return self;
}

- (int) spacing {
	return spacing;
}

- setSpacing: (int) newSpacing {
	spacing = newSpacing;
	[[NSNotificationCenter defaultCenter] 
		postNotificationName: @"OGGridVisualPropertyChanged" object: self];
	return self;
}

- (BOOL) snap {
	return snap;
}

- setSnap: (BOOL) newSnap {
	snap = newSnap;
	return self;
}

- (NSColor *) color {
	return color;
}

- setColor: (NSColor *) newColor {
	color = newColor;
	[[NSNotificationCenter defaultCenter] 
		postNotificationName: @"OGGridVisualPropertyChanged" object: self];
	return self;
}

- (BOOL) visible {
	return visible;
}

- setVisible: (BOOL) newVisible {
	visible = newVisible;
	[[NSNotificationCenter defaultCenter] 
		postNotificationName: @"OGGridVisualPropertyChanged" object: self];
	return self;
}

- (int) snapDistance {
	return snapDistance;
}

- setSnapDistance: (int) newSnapDistance {
	snapDistance = newSnapDistance;
	return self;
}

@end

