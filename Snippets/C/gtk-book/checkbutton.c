#include <gtk/gtk.h>

static void check1_toggled (GtkToggleButton *, GtkWidget *);
static void check2_toggled (GtkToggleButton *, GtkWidget *);

int main (int argc, char *argv[])
{
	GtkWidget *window, *vbox, *check1, *check2, *button_close;

	gtk_init (&argc, &argv);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Toggle Buttons");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);

	vbox = gtk_vbox_new (TRUE, 5);
	check1 = gtk_check_button_new_with_label ("Activate to enable to other one");
	check2 = gtk_check_button_new_with_label ("Activate to enable the button");
	gtk_widget_set_sensitive (check2, FALSE);

	button_close = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
	gtk_widget_set_sensitive (button_close, FALSE);

	g_signal_connect (G_OBJECT (check1), "toggled",
				G_CALLBACK (check1_toggled),
				(gpointer) check2);
	g_signal_connect (G_OBJECT (check2), "toggled",
				G_CALLBACK (check2_toggled),
				(gpointer) button_close);

	/* g_signal_connect_swapped (G_OBJECT (button_close), "clicked",
					G_CALLBACK (gtk_widget_destroy),
					(gpointer) window);
	*/

	g_signal_connect (G_OBJECT (button_close), "clicked",
				G_CALLBACK (gtk_main_quit),
				NULL);

	gtk_box_pack_start (GTK_BOX (vbox), check1, FALSE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), check2, FALSE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), button_close, FALSE, TRUE, 0);

	gtk_container_add (GTK_CONTAINER (window), vbox);
	gtk_widget_show_all (window);

	gtk_main ();
	return 0;
}

static void check1_toggled (GtkToggleButton *check1, GtkWidget *check2)
{
	if (gtk_toggle_button_get_active (check1))
		gtk_widget_set_sensitive (check2, TRUE);
	else
		gtk_widget_set_sensitive (check2, FALSE);
}

static void check2_toggled (GtkToggleButton *check2, GtkWidget *button_close)
{
	if (gtk_toggle_button_get_active (check2))
		gtk_widget_set_sensitive (button_close, TRUE);
	else
		gtk_widget_set_sensitive (button_close, FALSE);
}
