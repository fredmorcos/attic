#import <objective-gtk.h>
#import "general.h"
#import "file.h"
#import "layout.h"

@interface SBApp : Window {
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

