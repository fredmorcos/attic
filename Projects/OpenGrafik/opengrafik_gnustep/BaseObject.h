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

#import <AppKit/AppKit.h>

@class NSColor, NSString, NSBezierPath;

@interface BaseObject: NSView {
	NSString		*name;
	NSColor			*bgColor;
	NSColor			*fgColor;
	BOOL			selected;
	NSBezierPath	*path;
	NSSize			oldSize;
}

- (void) setName: (NSString *) newName;
- (void) setBackgroundColor: (NSColor *) color;
- (void) setForegroundColor: (NSColor *) color;
- (void) setSelected: (BOOL) sel;

- (NSString *) name;
- (NSColor *) backgroundColor;
- (NSColor *) foregroundColor;
- (BOOL) selected;

- (void) renderPath: (NSRect) rect;
- (BOOL) sizeHasChanged;

- (void) changedProperty: (NSString *) propName;

@end

