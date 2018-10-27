#include <objective-gtk/objective-gtk.h>

int main (int argc, char *argv[]) {
	[Gtk init: &argc: &argv];
	id window = [[Window alloc] init];
	[window showAll];
	[Gtk main];
	return 0;
}
