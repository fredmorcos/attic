#include <gtk/gtk.h>
#include <stdlib.h>

void scale_value_changed (GtkRange *range, gpointer user_data);

int
main (int argc, char *argv[])
{
	GtkWidget *win, *label, *scale, *hbox;

	gtk_init(&argc, &argv);

	win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_size_request(win, 300, 30);
	gtk_window_set_resizable(GTK_WINDOW(win), FALSE);
	gtk_container_set_border_width(GTK_CONTAINER(win), 5);

	label = gtk_label_new("Brightness:");
	scale = gtk_hscale_new_with_range(10, 100, 5);
	gtk_scale_set_draw_value(GTK_SCALE(scale), FALSE);

	hbox = gtk_hbox_new(FALSE, 5);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), scale, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(win), hbox);

	g_signal_connect(G_OBJECT(win), "delete-event", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(scale), "value-changed", G_CALLBACK(scale_value_changed), NULL);

	gtk_widget_show_all(win);
	gtk_main();

	return 0;
}

void scale_value_changed (GtkRange *range, gpointer user_data)
{
	static int value = 0;
	value = gtk_range_get_value(range);
	static char command[20];
	sprintf(command, "xbacklight -set %d", value);
	gboolean x = g_spawn_command_line_async(command, NULL);
}

