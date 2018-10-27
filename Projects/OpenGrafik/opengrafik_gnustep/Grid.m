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

- initWithFrame: (NSRect) frame {
	self = [super initWithFrame: frame];
	[self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
	cellSize = 10;
	blockSize = 10 * cellSize;
	
	return self;
}

- setCellSize: (int) size {
	cellSize = size;
	[self changedProperty: @"CellSize"];
	return self;
}

- setBlockSize: (int) size {
	blockSize = size * cellSize;
	[self changedProperty: @"BlockSize"];
	return self;
}

- (int) cellSize {
	return cellSize;
}

- (int) blockSize {
	return blockSize;
}

- (void) renderSmallGrid: (NSRect) rect {
	[path setLineWidth: 1.0];
	
	for (int i = 0; i <= rect.size.width; i += cellSize) {
		[path moveToPoint: NSMakePoint(i, 0)];
		[path lineToPoint: NSMakePoint(i, rect.size.height)];
	}
	
	for (int i = 0; i <= rect.size.height; i += cellSize) {
		[path moveToPoint: NSMakePoint(0, i)];
		[path lineToPoint: NSMakePoint(rect.size.width, i)];
	}
}

- (void) renderLargeGrid: (NSRect) rect {
	[path setLineWidth: 2.0];
	
	for (int i = 0; i <= rect.size.width; i += blockSize) {
		[path moveToPoint: NSMakePoint(i, 0)];
		[path lineToPoint: NSMakePoint(i, rect.size.height)];
	}
	
	for (int i = 0; i <= rect.size.height; i += blockSize) {
		[path moveToPoint: NSMakePoint(0, i)];
		[path lineToPoint: NSMakePoint(rect.size.width, i)];
	}
}

- (void) renderPath: (NSRect) rect {
	[fgColor set];
	[path removeAllPoints];
	[self renderSmallGrid: rect];
	[path stroke];
	[path removeAllPoints];
	[self renderLargeGrid: rect];
	[path stroke];
}

@end

