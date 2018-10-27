#include <gtk/gtk.h>

int main (int argc, char *argv[]) {
	GtkWidget *swindow, *button, *fixed, *window;
	
	gtk_init (&argc, &argv);
	
	swindow = gtk_scrolled_window_new (NULL, NULL);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_size_request (swindow, 200, 200);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	
	button = gtk_button_new_with_label ("HAHA! ANDREW!");
	gtk_widget_set_size_request (button, 100, 100);
	
	fixed = gtk_fixed_new ();
	gtk_widget_set_size_request (fixed, 300, 300);
	gtk_fixed_put (GTK_FIXED (fixed), button, 10, 10);
	
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (swindow), fixed);
	gtk_container_add (GTK_CONTAINER (window), swindow);
	
	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}
