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

#import "DiagramBaseObject.h"

@implementation DiagramBaseObject

- draw {
	return self;
}

/* Movable */
- moveTo: (NSPoint) newPoint {
	rect.origin = newPoint;
	return self;
}

- moveWithDelta: (NSPoint) deltaPoint {
	rect.origin.x += deltaPoint.x;
	rect.origin.y += deltaPoint.y;
	return self;
}

- moveXTo: (int) newX {
	rect.origin.x = newX;
	return self;
}

- moveXWithDelta: (int) deltaX {
	rect.origin.x += deltaX;
	return self;
}

- moveYTo: (int) newY {
	rect.origin.y = newY;
	return self;
}

- moveYWithDelta: (int) deltaY {
	rect.origin.y += deltaY;
	return self;
}

- (NSPoint) position {
	return rect.origin;
}

/* Resizable */
- resizeTo: (NSSize) newSize {
	rect.size = newSize;
	return self;
}

- resizeWithDelta: (NSSize) deltaSize {
	rect.size.height += deltaSize.height;
	rect.size.width += deltaSize.width;
	return self;
}

- resizeWidthTo: (int) newWidth {
	rect.size.width = newWidth;
	return self;
}

- resizeWidthWithDelta: (int) deltaWidth {
	rect.size.width += deltaWidth;
	return self;
}

- resizeHeightTo: (int) newHeight {
	rect.size.height = newHeight;
	return self;
}

- resizeHeightWithDelta: (int) deltaHeight {
	rect.size.height += deltaHeight;
	return self;
}

- (NSSize) size {
	return rect.size;
}

/* Rotatable */
- rotateTo: (int) newAngle {
	rotation = newAngle;
	return self;
}

- rotateWithDelta: (int) deltaAngle {
	rotation += deltaAngle;
	return self;
}

- resetRotation {
	rotation = 0;
	return self;
}

- (int) rotation {
	return rotation;
}

@end

