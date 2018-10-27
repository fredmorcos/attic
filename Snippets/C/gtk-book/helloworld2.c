#include <gtk/gtk.h>

static void destroy (GtkWidget*, gpointer);
// static gboolean delete_event (GtkWidget*, GdkEvent*, gpointer);

int main (int argc, char **argv)
{
	GtkWidget *window, *button; // *label;
	
	if (gtk_init_check (&argc, &argv) == FALSE) {
		g_print ("Sorry, cannot find X server.\n");
		return 0;
	}
	
	gtk_init (&argc, &argv);
		
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Hello Amsterdaaaam!!!");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	gtk_widget_set_size_request (window, 200, 100);
	
	button = gtk_button_new_with_mnemonic ("_Close");
	gtk_button_set_relief (GTK_BUTTON (button), GTK_RELIEF_NONE);
	
	/* label = gtk_label_new ("Hello Amsterdaaaam!!!");
	gtk_label_set_selectable (GTK_LABEL (label), TRUE);
	gtk_label_set_markup (GTK_LABEL (label), "<b>Hello</b> <i>Amsterdam!</i>");
	*/
	
	g_signal_connect (G_OBJECT (window), "destroy", 
				G_CALLBACK (destroy), NULL);
	/* g_signal_connect (G_OBJECT (window), "delete_event",
				G_CALLBACK (delete_event), 
				(gpointer) label); */
	g_signal_connect_swapped (G_OBJECT (button), "clicked",
				G_CALLBACK (gtk_widget_destroy),
				(gpointer) window);
	
	gtk_container_add (GTK_CONTAINER (window), button);
	gtk_widget_show_all (window);
	
	gtk_main ();
	return 0;
}

static void destroy (GtkWidget *window, gpointer data)
{
	gtk_main_quit ();
}

/* static gboolean delete_event (GtkWidget *window, GdkEvent *event, gpointer data)
{
	static num = 0;
	num++;
	if (num == 1) {
		gtk_label_set_text (GTK_LABEL (data), "Really leaving?!");
		return TRUE;
	}
	else {
		return FALSE;
	}
} */
