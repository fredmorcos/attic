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

#import <objective-gtk.h>
#import "general.h"
#import "file.h"
#import "layout.h"

@interface GApp : Window {
@public
	Notebook *notebook;
	
	GeneralBox *generalbox;
	FileBox *filebox;
	LayoutBox *layoutbox;

	HBox *mainbox;

	VSeparator *drawareasep;
	DrawingArea *drawarea;
}

- init;
- free;

@end

