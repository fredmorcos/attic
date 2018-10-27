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

@class NSColor, NSBezierPath, NSString, NSString;

@interface BaseObject: NSObject {
	NSString		*name;
	NSColor			*bgColor;
	NSColor			*fgColor;
	NSBezierPath	*path;
	NSRect			rect;
	int				rotation;
	BOOL			selected;
	BOOL			visible;
	id				parent;
}

- setName: (NSString *) newName;
- setBackgroundColor: (NSColor *) color;
- setForegroundColor: (NSColor *) color;
- setSize: (NSSize) size;
- setOrigin: (NSPoint) origin;
- setRotation: (int) rot;
- setSelected: (BOOL) sel;
- setVisible: (BOOL) vis;
- setParent: (id) par;

- (NSString *) name;
- (NSColor *) backgroundColor;
- (NSColor *) foregroundColor;
- (NSSize) size;
- (NSPoint) origin;
- (int) rotation;
- (BOOL) selected;
- (BOOL) visible;
- (id) parent;

- render;
- stroke;

- changedProperty: (NSString *) propName;

@end

