#include <gtk/gtk.h>

static void folder_changed (GtkFileChooser *, GtkFileChooser * /*, GtkLabel * */);
static void file_changed (GtkFileChooser *, GtkLabel *);

static void font_changed (GtkFontButton *, GtkWidget *);

int main (int argc, char *argv[]) {
	GtkWidget *window, *chooser1, *chooser2, *label, *vbox, *fbutton, *flabel;
	GtkFileFilter *filter1, *filter2;
	PangoFontDescription *initial_font;

	gtk_init (&argc, &argv);

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "File Chooser");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);

	label = gtk_label_new ("File here");
	flabel = gtk_label_new ("Font here");

	chooser1 = gtk_file_chooser_button_new ("Choose a folder",
											GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	chooser2 = gtk_file_chooser_button_new ("Choose a file",
											GTK_FILE_CHOOSER_ACTION_OPEN);

	initial_font = pango_font_description_from_string ("Sans Bold 12");
	gtk_widget_modify_font (flabel, initial_font);

	fbutton = gtk_font_button_new_with_font ("Sans Bold 12");
	gtk_font_button_set_title (GTK_FONT_BUTTON (fbutton), "Choose a Font");

	g_signal_connect (G_OBJECT (chooser1), "selection_changed",
						G_CALLBACK (folder_changed),
						(gpointer) chooser2 /*,
						(gpointer) label */);
	g_signal_connect (G_OBJECT (chooser2), "selection_changed",
						G_CALLBACK (file_changed),
						(gpointer) label);

	g_signal_connect (G_OBJECT (fbutton), "font_set",
						G_CALLBACK (font_changed),
						(gpointer) flabel);

	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser1),
											g_get_home_dir());
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser2),
											g_get_home_dir());

	filter1 = gtk_file_filter_new ();
	filter2 = gtk_file_filter_new ();
	gtk_file_filter_set_name (filter1, "Image Files");
	gtk_file_filter_set_name (filter2, "All Files");
	gtk_file_filter_add_pattern (filter1, "*.png");
	gtk_file_filter_add_pattern (filter1, "*.jpg");
	gtk_file_filter_add_pattern (filter1, "*.gif");
	gtk_file_filter_add_pattern (filter2, "*");

	gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (chooser1), filter1);
	gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (chooser2), filter2);

	vbox = gtk_vbox_new (FALSE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), chooser1);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), chooser2);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), label);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), fbutton);
	gtk_box_pack_start_defaults (GTK_BOX (vbox), flabel);

	gtk_container_add (GTK_CONTAINER (window), vbox);
	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}

static void folder_changed (GtkFileChooser *chooser1,
							GtkFileChooser *chooser2 /*,
							GtkLabel *label */) {
	gchar *folder = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser1));
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser2), folder);
	// gtk_label_set_text (label, folder);
	g_free (folder);
}

static void file_changed (GtkFileChooser *chooser2, GtkLabel *label) {
	gchar *file = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser2));
	gtk_label_set_text (label, file);
	g_free (file);
}

static void font_changed (GtkFontButton *button, GtkWidget *label) {
	const gchar *font, buffer[256];
	PangoFontDescription *desc;

	font = gtk_font_button_get_font_name (button);
	desc = pango_font_description_from_string (font);

	g_snprintf (buffer, sizeof (buffer), "Font: %s", font);
	gtk_label_set_text (GTK_LABEL (label), buffer);
	gtk_widget_modify_font (label, desc);
}
