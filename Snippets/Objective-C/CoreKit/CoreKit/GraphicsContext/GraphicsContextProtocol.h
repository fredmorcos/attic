/*
 *	This file is part of CoreKit.
 *
 *	Copyright 2009 Fred Morcos <fred.morcos@gmail.com>
 *
 *	CoreKit is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	CoreKit is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with CoreKit.  If not, see <http://www.gnu.org/licenses/>.
 */

#import <Foundation/Foundation.h>

#define NSRed(x) ((float)x.r/255.0)
#define NSGreen(x) ((float)x.g/255.0)
#define NSBlue(x) ((float)x.b/255.0)
#define NSAlpha(x) ((float)x.a/255.0)
#define NSMakeColor(r,g,b,a) ({r,g,b,a})

#define NSLine NSRect

typedef struct NSColor {
	unsigned char r, g, b, a;
} NSColor;

@protocol GraphicsContextProtocol

- setFillColor: (NSColor) col;
- (NSColor) fillColor;
- setBorderColor: (NSColor) col;
- (NSColor) borderColor;
- setLineWidth: (int) lw;
- (int) lineWidth;
- setPosition: (NSPoint) pnt;
- (NSPoint) position;
- setBorder: (BOOL) val;
- (BOOL) border;
- setFill: (BOOL) val;
- (BOOL) fill;

- drawRect: (NSRect) rect;
- drawRoundRect: (NSRect) rect roundness: (int) val;
- drawLine: (NSLine) line;

@end

