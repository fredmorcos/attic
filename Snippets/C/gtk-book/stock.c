#include <gtk/gtk.h>

int main (int argc, char *argv[])
{
	GtkWidget *window, *table, *button;

	gtk_init (&argc, &argv);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Stock!");
	gtk_widget_set_size_request (window, 400, 200);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);

	table = gtk_table_new (4, 4, TRUE);
	
	button = gtk_button_new_from_stock (GTK_STOCK_CLOSE);

	gtk_table_attach_defaults (GTK_TABLE (table), button, 0, 1, 0, 1);

	g_signal_connect_swapped (G_OBJECT (button), "clicked",
					G_CALLBACK (gtk_widget_destroy),
					(gpointer) window);

	gtk_container_add (GTK_CONTAINER (window), table);
	gtk_widget_show_all (window);

	gtk_main ();
	return 0;
}
