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
#import <Renaissance/Renaissance.h>
#import "ApplicationController.h"

int main (int argc, const char **argv) {
	CREATE_AUTORELEASE_POOL (pool);

	[NSApplication sharedApplication];
	[NSApp setDelegate: [ApplicationController new]];

#ifdef GNUSTEP
	[NSBundle loadGSMarkupNamed: @"Menu-GNUstep" owner: [NSApp delegate]];
#else
	[NSBundle loadGSMarkupNamed: @"Menu-OSX" owner: [NSApp delegate]];
#endif

	RELEASE (pool);

	return NSApplicationMain(argc, argv);
}

