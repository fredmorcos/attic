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

#import "DiagramObject.h"

@implementation DiagramObject

- setWidth: (int) w height: (int) h {
	rect.size.width = w;
	rect.size.height = h;
	[self notifyChange];
	return self;
}

- (int) getWidth {
	return (int) rect.size.width;
}

- (int) getHeight {
	return (int) rect.size.height;
}

- setX: (int) x andY: (int) y {
	rect.origin.x = x;
	rect.origin.y = y;
	[self notifyChange];
	return self;
}

- (int) getX {
	return (int) rect.origin.x;
}

- (int) getY {
	return (int) rect.origin.y;
}

- setPosition: (NSPoint) position {
	rect.origin = position;
	[self notifyChange];
	return self;
}

- (NSPoint) position {
	return rect.origin;
}

- setSize: (NSSize) size {
	rect.size = size;
	[self notifyChange];
	return self;
}

- (NSSize) size {
	return rect.size;
}

- setRect: (NSRect) r {
	rect = r;
	return self;
}

- (NSRect) rect {
	return rect;
}

@end

