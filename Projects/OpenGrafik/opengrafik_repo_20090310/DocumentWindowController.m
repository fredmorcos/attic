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

#import "DocumentWindowController.h"
#import "config.h"
#import <Renaissance/Renaissance.h>

@implementation DocumentWindowController

- init {
	self = [super init];
	[NSBundle loadGSMarkupNamed: @"DocumentWindow" owner: self];
	[self resetWindowFrame];
	return self;
}

- resetWindowFrame {
	NSRect	screenFrame,
			windowFrame;

	screenFrame = [[window screen] frame];
	windowFrame = [window frame];

	windowFrame.size.width = screenFrame.size.width * 3 / 4;
	windowFrame.size.height = screenFrame.size.height * 3 / 4;
	
	windowFrame.origin.x = SPACING * 8;
	windowFrame.origin.y = SPACING * 8;
	
	[window setFrame: windowFrame display: YES animate: NO];
	
	return self;
}

@end

