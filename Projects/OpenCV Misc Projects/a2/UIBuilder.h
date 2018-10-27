#import <objc/Object.h>
#import <gtk/gtk.h>

@interface UIBuilder: Object {
@protected
	GtkBuilder *builder;
}

- (BOOL) addFromFile: (const char *) filename;
- (GtkWidget *) getWidget: (const char *) name;
- free;

@end
