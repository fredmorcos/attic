#include <gtk/gtk.h>

int main (int argc, char *argv[])
{
	GtkWidget *window, *table, *label1, *label2, *entry;
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Panes");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	gtk_widget_set_size_request (window, 150, 100);
	
	table = gtk_table_new (2, 2, TRUE);
	label1 = gtk_label_new ("Enter the following info:");
	label2 = gtk_label_new ("Name:");
	entry = gtk_entry_new ();
	
	gtk_table_attach (GTK_TABLE (table), label1, 0, 2, 0, 1,
				GTK_EXPAND, GTK_SHRINK, 0, 0);
	gtk_table_attach (GTK_TABLE (table), label2, 0, 1, 1, 2,
				GTK_EXPAND, GTK_SHRINK, 0, 0);
	gtk_table_attach (GTK_TABLE (table), entry, 1, 2, 1, 2,
				GTK_EXPAND, GTK_SHRINK, 0, 0);
	
	gtk_table_set_row_spacings (GTK_TABLE (table), 5);
	gtk_table_set_col_spacings (GTK_TABLE (table), 5);
	
	gtk_container_add (GTK_CONTAINER (window), table);
	gtk_widget_show_all (window);
	
	gtk_main ();
	return 0;
}
