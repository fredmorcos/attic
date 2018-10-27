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
		timesteplabel = [[[Label alloc] init] text: "Timestep:"];
		timestepspin = [[[[SpinButton alloc] init] range: 0: 1000] step: 1: 5];
		timebox = [[[[HBox alloc] init]
			packStart: timesteplabel: NO: NO: 5] 
			packStart: timestepspin: YES: YES: 0];
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
		
		[[[[[[[[[self 
			packStart: typecombo]
			packStart: coulbox] packStart: springbox] packStart: timebox]
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
	[timesteplabel free];
	[timestepspin free];
	[timebox free];
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

