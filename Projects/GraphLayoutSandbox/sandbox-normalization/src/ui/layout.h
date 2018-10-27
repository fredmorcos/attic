#import <objective-gtk.h>

@interface LayoutBox: VBox {
@public
	Label *coulconstlabel;
	SpinButton *coulconstspin;
	HBox *coulbox;
	Label *springconstlabel;
	SpinButton *springconstspin;
	HBox *springbox;
	Label *timesteplabel;
	SpinButton *timestepspin;
	HBox *timebox;
	Label *dampinglabel;
	SpinButton *dampingspin;
	HBox *dampbox;
	Label *looplabel;
	SpinButton *loopspin;
	HBox *loopbox;
	HSeparator *algsep;

	CheckButton *gravitycheck;
	ToggleButton *walltog;

	ProgressBar *execpb;
	Button *execute;
}

- init;
- free;

@end

