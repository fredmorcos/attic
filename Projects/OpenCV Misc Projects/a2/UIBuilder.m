#import "UIBuilder.h"
#import <gtk/gtk.h>

@implementation UIBuilder

/**
 * Adds a UI XML description from file.
 */
- (BOOL) addFromFile: (const char *) filename {
	if (!builder) builder = gtk_builder_new();
	return gtk_builder_add_from_file(builder, filename, NULL);
}

/**
 * Gets a widget named name.
 */
- (GtkWidget *) getWidget: (const char *) name {
	return GTK_WIDGET(gtk_builder_get_object(builder, name));
}

/**
 * UIBuilder destructor.
 */
- free {
	g_object_unref(builder);
	return [super free];
}

@end

