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

#import "Grid.h"

@implementation Grid

- init {
	self = [super init];
	cellSize = 20;
	return self;
}

- setCellSize: (int) size {
	cellSize = size;
	[self changedProperty: @"CellSize"];
	return self;
}

- (int) cellSize {
	return cellSize;
}

- render {
	if (visible == YES) {
		for (int i = cellSize; i < rect.size.width; i += cellSize) {
			[path moveToPoint: NSMakePoint(i, 0)];
			[path lineToPoint: NSMakePoint(i, rect.size.height)];
		}
		
		for (int i = cellSize; i < rect.size.height; i += cellSize) {
			[path moveToPoint: NSMakePoint(0, i)];
			[path lineToPoint: NSMakePoint(rect.size.width, i)];
		}
	}
	return self;
}

- stroke {
	[NSBezierPath clipRect: rect];
	[bgColor set];
	[NSBezierPath fillRect: rect];
	[fgColor set];
	[path stroke];
	return self;
}

@end

