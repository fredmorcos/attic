#include <gtk/gtk.h>
#include <stdio.h>

static GtkWidget *image_text_box (gchar *, gchar *);
gboolean exit_program (void);
static void button_clicked (GtkWidget *, gpointer);

int main (int argc, char *argv[])
{
	GtkWidget	*window;
	GtkWidget	*button;
	GtkWidget	*box;
	
	gtk_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW(window), "Test");
	
	g_signal_connect (	G_OBJECT(window), "destroy",
						G_CALLBACK(exit_program), NULL);
	g_signal_connect (	G_OBJECT(window), "delete-event",
						G_CALLBACK(exit_program), NULL);
						
	gtk_container_set_border_width (GTK_CONTAINER(window), 2);
	
	button = gtk_button_new ();
	
	g_signal_connect (	G_OBJECT(button), "clicked",
						G_CALLBACK(button_clicked), (gpointer) "cool button!!!");
						
	box = image_text_box ("info.xpm", "cool button!");
	
	gtk_container_add (GTK_CONTAINER(button), box);
	
	gtk_container_add (GTK_CONTAINER(window), button);
	
	gtk_widget_show_all (GTK_WIDGET(window));
	
	gtk_main ();
	return 0;
}

static void button_clicked (GtkWidget *widget, gpointer data)
{
	printf ("hi, %s was clicked\n", (char *) data);
}

gboolean exit_program ()
{
	gtk_main_quit ();
	return FALSE;
}

static GtkWidget *image_text_box (	gchar	*image_filename,
									gchar	*label_text)
{
	GtkWidget	*box;
	GtkWidget	*text;
	GtkWidget	*image;
	
	box = gtk_hbox_new (FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER(box), 2);
	
	image = gtk_image_new_from_file (image_filename);
	
	text = gtk_label_new (label_text);
	
	gtk_box_pack_start (GTK_BOX(box), image, FALSE, FALSE, 3);
	gtk_box_pack_start (GTK_BOX(box), text, FALSE, FALSE, 3);
	
	gtk_widget_show (image);
	gtk_widget_show (text);
	
	return box;
}
