/*
	This file is part of Grafer.
	
	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License version 3 
	as published by	the Free Software Foundation.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer. If not, see <http://www.gnu.org/licenses/>.
*/

#import "app.h"

@implementation GApp

- init {
	if ((self = [super init])) {
		generalbox = [[GeneralBox alloc] init];
		filebox = [[FileBox alloc] init];
		layoutbox = [[LayoutBox alloc] init];

		drawareasep = [[VSeparator alloc] init];
		drawarea = [[[[DrawingArea alloc] init] 
			doubleBuffered: YES] redrawOnAllocate: YES];

		mainbox = [[[[HBox alloc] init] homogeneous: NO] spacing: 2];

		notebook = [[Notebook alloc] init];
		[notebook appendPage: generalbox: [[[Label alloc] init] text: "General"]];
		[notebook appendPage: filebox: [[[Label alloc] init] text: "File"]];
		[notebook appendPage: layoutbox: [[[Label alloc] init] text: "Layout"]];

		[[[mainbox 
			packStart: notebook] 
			packStart: drawareasep: NO: NO: 5]
			packStart: drawarea: YES: YES: 0];

		[[[[[self borderWidth: 2] defaultSize: 800: 600] add: mainbox] showAll] 
				   onDestroy: (GCallback) gtk_main_quit: NULL];
	}
	return self;
}

- free {
	[generalbox free];
	[filebox free];
	[layoutbox free];
	[notebook free];

	g_free(mainbox);
	g_free(drawarea);
	return [super free];
}

@end

