#import <objective-gtk.h>

@interface LayoutBox: VBox {
@public
	ComboBox 		*typecombo;
	Label			*coulconstlabel;
	SpinButton		*coulconstspin;
	HBox			*coulbox;
	Label			*springconstlabel;
	SpinButton		*springconstspin;
	HBox			*springbox;
	Label			*timesteplabel;
	SpinButton		*timestepspin;
	HBox			*timebox;
	Label			*dampinglabel;
	SpinButton		*dampingspin;
	HBox			*dampbox;
	HSeparator		*algsep;

	ToggleButton	*walltog;
	Button			*execute;
	Button			*stop;
}

- init;
- free;

@end

