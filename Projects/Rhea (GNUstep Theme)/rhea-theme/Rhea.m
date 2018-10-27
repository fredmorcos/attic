/*	
 *	This file is part of Rhea.
 *
 *	Copyright 2009	Frederic Morcos <fred.morcos@gmail.com>
 *
 *	Rhea is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Rhea is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Rhea.  If not, see <http://www.gnu.org/licenses/>.
 */

#import "Rhea.h"

#import <AppKit/AppKit.h>

@implementation Rhea

- (void) drawButton: (NSRect) frame 
				 in: (NSCell*) cell 
			   view: (NSView*) view 
			  style: (int) style 
			  state: (GSThemeControlState) state {
	if (state == GSThemeNormalState)
		[[NSColor blackColor] set];
	else if (state == GSThemeHighlightedState)
		[[NSColor whiteColor] set];
	else if (state == GSThemeSelectedState)
		[[NSColor redColor] set];
	[[NSBezierPath bezierPathWithRect: frame] stroke];
}

@end

