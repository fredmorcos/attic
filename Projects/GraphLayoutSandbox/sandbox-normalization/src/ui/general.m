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

		scalelabel = [[[Label alloc] init] text: "Scale:"];
		scale = [[[[[[HScale alloc] init] range: 0.1: 5.0] increments: 0.1: 0.5] 
								  valuePosition: GTK_POS_RIGHT] value: 1.0];
		scalebox = [[[[HBox alloc] init]
			packStart: scalelabel: NO: NO: 5] packStart: scale: YES: YES: 0];
		scalesep = [[HSeparator alloc] init];

		[[[[[[[[[self 
			packStart: nodebox] packStart: nodeapply] packStart: infolabel]
			packStart: nodesep: NO: NO: 5] packStart: edgebox] packStart: edgeapply]
			packStart: edgesep: NO: NO: 5] packStart: scalebox] 
			packStart: scalesep: NO: NO: 5];
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
	[scalelabel free];
	[scale free];
	[scalebox free];

	return [super free];
}

@end

