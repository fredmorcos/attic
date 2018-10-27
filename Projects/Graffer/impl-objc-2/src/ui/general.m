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

#import "general.h"

@implementation GeneralBox

- init {
	if ((self = [super init])) {
		[[self homogeneous: NO] spacing: 5];

		nodelabel = [[[Label alloc] init] text: "Nodes:"];
		nodespin = [[[[SpinButton alloc] init] range: 1: 100000] step: 1: 5];
		nodebox = [[[[[[HBox alloc] init] homogeneous: NO] spacing: 5] 
			  packStart: nodelabel: NO: NO: 5] packStart: nodespin: YES: YES: 0];
		nodeapply = [[[[Button alloc] init] label: "gtk-apply"] stock: YES];
		infolabel = [[[Label alloc] init] 
			text: "Right-Click a node to lock \nor unlock it's position"];
		nodesep = [[HSeparator alloc] init];

		edgelabel = [[[Label alloc] init] text: "Edges:"];
		edgecombo = [[[[[[[[ComboBox alloc] init] 
			appendText: "None"] appendText: "Circular"] appendText: "Centered"]
			appendText: "Interconnected"] appendText: "Binary Tree"] setActive: 0];
		edgebox = [[[[[[HBox alloc] init] homogeneous: NO] spacing: 2]
			  packStart: edgelabel: NO: NO: 5] packStart: edgecombo: YES: YES: 0];
		edgeapply = [[[[Button alloc] init] label: "gtk-apply"] stock: YES];
		edgesep = [[HSeparator alloc] init];

		[[[[[[[self 
			packStart: nodebox] packStart: nodeapply] packStart: infolabel]
			packStart: nodesep: NO: NO: 5] packStart: edgebox] packStart: edgeapply]
			packStart: edgesep: NO: NO: 5];
	}
	
	return self;
}

- free {
	[nodesep free];
	[infolabel free];
	[nodeapply free];
	[nodespin free];
	[nodelabel free];
	[nodebox free];
	[edgelabel free];
	[edgecombo free];
	[edgebox free];
	[edgeapply free];
	[edgesep free];

	return [super free];
}

@end

