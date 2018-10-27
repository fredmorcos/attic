#include <gtk/gtk.h>

int main (int argc, char *argv[])
{
	gint i;
	GtkWidget *window, *handle, *label;
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Boxes");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	gtk_widget_set_size_request (window, 200, -1);
	
	handle = gtk_handle_box_new ();
	label = gtk_label_new ("Deatch me!!!");

	gtk_handle_box_set_shadow_type (GTK_HANDLE_BOX (handle), GTK_SHADOW_ETCHED_OUT);
	gtk_handle_box_set_handle_position (GTK_HANDLE_BOX (handle), GTK_POS_RIGHT);
	gtk_handle_box_set_snap_edge (GTK_HANDLE_BOX (handle), GTK_POS_TOP);

	gtk_container_add (GTK_CONTAINER (handle), label);
	gtk_container_add (GTK_CONTAINER (window), handle);
	gtk_widget_show_all (window);
	
	gtk_main ();
	return 0;
}
