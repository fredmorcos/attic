#include <gtk/gtk.h>

void cb_composited (GtkWidget *, gpointer);
gboolean composited (GdkScreen *);

int main (int argc, char *argv[]) {
	GtkWidget *window, *label;
	GdkScreen *screen = gdk_screen_get_default ();
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Composite Check");
	
	if (composited (screen))
		label = gtk_label_new ("Screen composited!");
	else
		label = gtk_label_new ("Screen not composited!");
		
	gtk_container_add (GTK_CONTAINER (window), label);
	
	g_signal_connect (G_OBJECT (screen), "composited_changed",
						G_CALLBACK (cb_composited), (gpointer) label);
	
	gtk_widget_show_all (window);
	gtk_main ();	
	return 0;
}

void cb_composited (GtkWidget *screen, gpointer label) {
	if (composited (GDK_SCREEN (screen)))
		gtk_label_set_label (GTK_LABEL (label), "Screen composited!");
	else
		gtk_label_set_label (GTK_LABEL (label), "Screen not composited!");
}

gboolean composited (GdkScreen *screen) {
	if (gdk_screen_is_composited (GDK_SCREEN (screen)))
		return TRUE;
	else
		return FALSE;
}
