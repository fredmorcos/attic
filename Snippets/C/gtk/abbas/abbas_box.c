#include<gtk/gtk.h>

void quit (void);
void abbas_who (void);

int main(int argc, char *argv[]) {
	GtkWidget *message_window;
	GtkWidget *abbas_who_button;
	GtkWidget *message_label;
	GtkWidget *box;
	
	gtk_init (&argc, &argv);
	
	message_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (message_window), "The Sacred Question");
	// gtk_window_set_size (GTK_WINDOW (message_window), 200, 100);
	// gtk_window_set_resizable (GTK_WINDOW (message_window), FALSE);
	gtk_window_set_position (GTK_WINDOW (message_window), GTK_WIN_POS_CENTER_ALWAYS);
	g_signal_connect (G_OBJECT (message_window), "destroy", G_CALLBACK (quit), NULL);
	
	box = gtk_vbox_new(TRUE, 0);
	abbas_who_button = gtk_button_new_with_label ("Abbas Who?!");
	g_signal_connect (G_OBJECT (abbas_who_button), "clicked", G_CALLBACK (abbas_who), NULL);
	
	message_label = gtk_label_new ("Do you know abbas?");
	
	gtk_container_border_width (GTK_CONTAINER (message_window), 4);
	gtk_container_add (GTK_CONTAINER (message_window), box);
	gtk_box_pack_start (GTK_BOX(box), message_label, TRUE, TRUE, 4);
	gtk_box_pack_start (GTK_BOX(box), abbas_who_button, TRUE, FALSE, 4);
	
	gtk_widget_show_all (message_window);
	gtk_main ();
	
	return 0;
}

void quit () {
	gtk_main_quit ();
	/* gtk_exit (0); */
}

void abbas_who () {
	g_print ("*\n*\nELLI BEYNEEEEEEEEEEEEKAAAAAAAAAAAAK\n*\n*\n");
	quit ();
}
