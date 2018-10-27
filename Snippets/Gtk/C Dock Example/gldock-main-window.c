#include <gtk/gtk.h>

int main (int argc, char *argv[]) {
	GtkWidget *window;
	GdkColor bg_color;

	gtk_init (&argc, &argv);

	gdk_color_parse ("#FFFFFF", &bg_color);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_decorated (GTK_WINDOW (window), FALSE);
	gtk_widget_set_size_request (window, 300, 30);
	gtk_widget_modify_bg (window, GTK_STATE_NORMAL, &bg_color);

	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}
