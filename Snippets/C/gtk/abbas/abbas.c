#include<gtk/gtk.h>
#include<stdlib.h>
#include"abbas.h"

GtkWidget *message_window;
GtkWidget *abbas_who_button;
GtkWidget *yes_button;
GtkWidget *message_label;
GtkWidget *table;
GtkWidget *message;

void create_abbasation() {
	
	message_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (message_window), "The Sacred Question");
	gtk_window_resize (GTK_WINDOW (message_window), 300, 50);
	// gtk_window_set_resizable (GTK_WINDOW (message_window), FALSE);
	gtk_window_set_position (GTK_WINDOW (message_window), GTK_WIN_POS_CENTER_ALWAYS);
	g_signal_connect (G_OBJECT (message_window), "destroy", G_CALLBACK (quit), NULL);
	
	abbas_who_button = gtk_button_new_with_label ("Abbas meen?!");
	g_signal_connect (G_OBJECT (abbas_who_button), "clicked", G_CALLBACK (abbas_who), NULL);
	
	yes_button = gtk_button_new_with_label ("Of Course!");
	g_signal_connect (G_OBJECT (yes_button), "enter", G_CALLBACK (yes_button_on_over), NULL);
	g_signal_connect (G_OBJECT (yes_button), "leave", G_CALLBACK (yes_button_on_leave), NULL);
	
	message_label = gtk_label_new ("Do you know abbas?!");
	
	table = gtk_table_new (2, 2, TRUE);
	gtk_table_set_row_spacings (GTK_TABLE (table), 2);
	gtk_table_set_col_spacings (GTK_TABLE (table), 2);
	gtk_table_attach_defaults (GTK_TABLE (table), message_label, 0, 2, 0, 1);
	gtk_table_attach_defaults (GTK_TABLE (table), yes_button, 0, 1, 1, 2);
	gtk_table_attach_defaults (GTK_TABLE (table), abbas_who_button, 1, 2, 1, 2);
	gtk_container_border_width (GTK_CONTAINER (message_window), 2);
	gtk_container_add (GTK_CONTAINER (message_window), table);
	
	gtk_widget_show_all (message_window);
}

void quit () {
	/* message = gtk_message_dialog_new (GTK_WINDOW (message_window),
										GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
										GTK_BUTTONS_OK, "NO NO NO :D");
	gtk_dialog_run (GTK_DIALOG (message)); */
	/* a dirty hack after the destroy signal has been sent 
	 * to restart the main window
	 */
	main(0, 0);
}

void abbas_who () {
	/* g_print ("*\n*\nELLI BEYNEEEEEEEEEEEEKAAAAAAAAAAAAK\n*\n*\n"); */
	/* quit (); */
	message = gtk_message_dialog_new (GTK_WINDOW (message_window),
										GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING,
										GTK_BUTTONS_OK, "EL BEYNEEEEEEKAK YA BRENCE!");
	gtk_dialog_run (GTK_DIALOG (message));
	gtk_main_quit ();
	exit(0);
}

void yes_button_on_over () {
	// g_print ("\ninside yes button!\n");
	gtk_button_set_label (GTK_BUTTON (yes_button), "Zebbi 3aleik");
	gtk_widget_set_sensitive (yes_button, FALSE);
}

void yes_button_on_leave () {
	// g_print ("\nleaving yes button!\n");
	gtk_button_set_label (GTK_BUTTON (yes_button), "Of Course!");
	gtk_widget_set_sensitive (yes_button, TRUE);
}
