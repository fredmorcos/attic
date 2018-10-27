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

#import "layout.h"

@implementation LayoutBox

- init {
	if ((self = [super init])) {
		[[self homogeneous: NO] spacing: 2];
		
		typecombo = [[[[[[[[[ComboBox alloc] init] appendText: "Simple"]
						appendText: "Fruchterman-Reingold"] appendText: "Eades"]
						appendText: "GEM"] appendText: "Grid"]
						appendText: "Barnes-Hut"] setActive: 0];
		
		coulconstlabel = [[[Label alloc] init] text: "Electr Const:"];
		coulconstspin = [[[[SpinButton alloc] init] range: 0: 1000000] step: 1: 5];
		coulbox = [[[[HBox alloc] init] 
			packStart: coulconstlabel: NO: NO: 5] 
			packStart: coulconstspin: YES: YES: 0];
		springconstlabel = [[[Label alloc] init] text: "Spring Const:"];
		springconstspin = [[[[SpinButton alloc] init] range: 0: 1000000] step: 1: 5];
		springbox = [[[[HBox alloc] init]
			packStart: springconstlabel: NO: NO: 5] 
			packStart: springconstspin: YES: YES: 0];
		dampinglabel = [[[Label alloc] init] text: "Damping:"];
		dampingspin = [[[[SpinButton alloc] init] range: 0: 10] step: 0: 5];
		dampbox = [[[[HBox alloc] init]
			packStart: dampinglabel: NO: NO: 5] 
			packStart: dampingspin: YES: YES: 0];
		algsep = [[HSeparator alloc] init];

		walltog = [[[ToggleButton alloc] init] label: "Wall"];
		execute = [[[[Button alloc] init] label: "gtk-execute"] stock: YES];
		stop = [[[[[Button alloc] init] label: "gtk-stop"] 
										stock: YES] sensitive: NO];
		
		[[[[[[[[self 
			packStart: typecombo] packStart: coulbox] packStart: springbox]
			packStart: dampbox] packStart: algsep: NO: NO: 5]
			packStart: walltog] packStart: execute] packStart: stop];
	}
	
	return self;
}

- free {
	[typecombo free];
	[coulconstlabel free];
	[coulconstspin free];
	[coulbox free];
	[springconstlabel free];
	[springconstspin free];
	[springbox free];
	[dampinglabel free];
	[dampingspin free];
	[dampbox free];
	[algsep free];
	[walltog free];
	[execute free];
	[stop free];
	return [super free];
}

@end

