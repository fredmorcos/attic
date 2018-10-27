#include "gtk_login.h"

void login_finish (void);

int main (int argc, char *argv[])
{
	GtkWindow	*login_window;
	GtkTable	*main_table;
	GtkWidget *icon;
	GtkWidget *exit_button;
	GtkWidget *login_button;
	GtkWidget *username_label;
	GtkWidget *username_text;
	GtkWidget *server_label;
	GtkWidget *server_text;
	GtkWidget *login_table;
	GtkWidget *entry_table;
	GtkWidget *button_table;
	
	gtk_init(&argc, &argv);
	
	/* window */
	login_window = GTK_WINDOW (gtk_window_new (GTK_WINDOW_TOPLEVEL));
	gtk_window_set_title (login_window, "JbIRC Login");
	gtk_window_resize (login_window, 800, 200);
	gtk_window_set_position (login_window, GTK_WIN_POS_CENTER);
	gtk_window_set_resizable (login_window, FALSE);
	gtk_container_border_width (GTK_CONTAINER (login_window), 1);
	g_signal_connect (G_OBJECT (login_window), "destroy", G_CALLBACK (login_finish), NULL);
	
	/* table */
	main_table = GTK_TABLE (gtk_table_new (3, 3, FALSE));
	gtk_table_set_row_spacings (main_table, 2);
	gtk_table_set_col_spacings (main_table, 2);
	
	gtk_container_add (GTK_CONTAINER (login_window), GTK_WIDGET (main_table));
	
	/* login button */
	
	
	gtk_widget_show_all (GTK_WIDGET (login_window));
	gtk_main();
	return 0;
}

void login_finish ()
{
	/* unload libjbirc here */
	gtk_main_quit ();
}
