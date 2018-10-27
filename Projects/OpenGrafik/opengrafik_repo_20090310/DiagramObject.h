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

@class DiagramBaseObject;

@interface DiagramObject: DiagramBaseObject {
	NSRect	rect;
}

- setWidth: (int) w height: (int) h;
- (int) getWidth;
- (int) getHeight;
- setX: (int) x andY: (int) y;
- (int) getX;
- (int) getY;
- setPosition: (NSPoint) position;
- (NSPoint) position;
- setSize: (NSSize) size;
- (NSSize) size;
- setRect: (NSRect) r;
- (NSRect) rect;

@end

