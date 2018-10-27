#import "layout.h"

@implementation LayoutBox

- init {
	if ((self = [super init])) {
		[[self homogeneous: NO] spacing: 2];
		
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
		looplabel = [[[Label alloc] init] text: "Iterate:"];
		loopspin = [[[[SpinButton alloc] init] range: 1: 1000000] step: 1: 5];
		loopbox = [[[[HBox alloc] init] 
			packStart: looplabel: NO: NO: 5] 
			packStart: loopspin: YES: YES: 0];
		algsep = [[HSeparator alloc] init];

		gravitycheck = [[[CheckButton alloc] init] label: "Gravity"];		
		walltog = [[[ToggleButton alloc] init] label: "Wall"];

		execpb = [[ProgressBar alloc] init];
		execute = [[[[Button alloc] init] label: "gtk-execute"] stock: YES];
		
		[[[[[[[[[[self 
			packStart: coulbox] packStart: springbox] packStart: timebox]
			packStart: dampbox] packStart: loopbox]
			packStart: algsep: NO: NO: 5]
			packStart: walltog] packStart: gravitycheck]
			packStart: execpb] packStart: execute];
	}
	
	return self;
}

- free {
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
	[looplabel free];
	[loopspin free];
	[loopbox free];
	[algsep free];
	[gravitycheck free];
	[walltog free];
	[execpb free];
	[execute free];
	return [super free];
}

@end

