#include <gtk/gtk.h>

static void button_clicked (GtkButton *, GtkWindow *);

int main (int argc, char *argv[]) {
	GtkWidget *window, *button;

	gtk_init (&argc, &argv);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Dialogs");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);

	button = gtk_button_new_with_mnemonic ("_Click Me!");

	g_signal_connect (G_OBJECT (button), "clicked",
						G_CALLBACK (button_clicked),
						(gpointer) window);

	gtk_container_add (GTK_CONTAINER (window), button);
	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}

static void button_clicked (GtkButton *button, GtkWindow *parent) {
	GtkWidget *dialog, *label, *image, *hbox;

	dialog = gtk_dialog_new_with_buttons ("Information", parent,
											/* GTK_DIALOG_MODAL, */
											GTK_DIALOG_DESTROY_WITH_PARENT,
											GTK_STOCK_OK, GTK_RESPONSE_OK,
											NULL);

	gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

	label = gtk_label_new ("The button was clicked!");
	image = gtk_image_new_from_stock (GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_DND);

	hbox = gtk_hbox_new (FALSE, 5);
	gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), image);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), label);

	gtk_box_pack_start_defaults (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox);
	gtk_widget_show_all (dialog);

	/* 
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
	*/
	
	g_signal_connect (G_OBJECT (dialog), "response", G_CALLBACK (gtk_widget_destroy), NULL);
}
