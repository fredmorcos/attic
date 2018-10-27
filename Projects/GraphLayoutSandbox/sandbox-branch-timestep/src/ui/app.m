#import "app.h"

@implementation SBApp

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

