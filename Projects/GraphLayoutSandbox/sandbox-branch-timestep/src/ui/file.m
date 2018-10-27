#import "file.h"

@implementation FileBox

- init {
	if ((self = [super init])) {
		[[self homogeneous: NO] spacing: 5];
		
		save = [[[[Button alloc] init] label: "gtk-save"] stock: YES];
		quitbut = [[[[[Button alloc] init] label: "gtk-quit"] stock: YES] 
			onClicked: (GCallback) gtk_main_quit: NULL];
			
		[[self packStart: save] packStart: quitbut];
	}
	
	return self;
}

- free {
	[save free];
	[quitbut free];
	return [super free];
}

@end

