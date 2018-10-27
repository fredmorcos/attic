#import <objective-gtk.h>

@interface GeneralBox: VBox {
@public
	HBox		*nodebox;
	Label 		*nodelabel;
	SpinButton 	*nodespin;
	Button 		*nodeapply;
	Label 		*infolabel;
	HSeparator 	*nodesep;

	Label 		*edgelabel;
	ComboBox 	*edgecombo;
	HBox 		*edgebox;
	Button 		*edgeapply;
	HSeparator	*edgesep;
}

- init;
- free;

@end

