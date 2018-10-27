#include <gtk/gtk.h>

static void msg_button_clicked (GtkButton *, GtkWindow *);
static void abt_button_clicked (GtkButton *, GtkWindow *);
static void file_button_clicked (GtkButton *, GtkWindow *);
static void choose_folder_button_clicked (GtkButton *, GtkWindow *);
static void mulfile_button_clicked (GtkButton *, GtkWindow *);
static void run_color_dialog (GtkButton *, GtkWindow *);
static void run_color_selection_dialog (GtkButton *, GtkWindow *, gboolean);
static void dialog_response (GtkDialog *, gint, gpointer);

static GdkColor global_color;
static guint global_alpha = 65535;

int main (int argc, char *argv[]) {
	GtkWidget *window, *msg_button, *hbox, *abt_button, *file_button, 
				*choose_folder_button, *mulfile_button, *color_button;
	gint i = 1;
	
	gtk_init (&argc, &argv);
	
	while (i < argc)
		if (gdk_color_parse (argv[i], &global_color))
			break;
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (window), "Message Dialog");
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	
	msg_button = gtk_button_new_with_mnemonic ("_Click Me");
	abt_button = gtk_button_new_with_mnemonic ("_About Me");
	file_button = gtk_button_new_with_label ("Choose file");
	choose_folder_button = gtk_button_new_with_label ("Choose folder to create");
	mulfile_button = gtk_button_new_with_label ("Choose multiple files...");
	color_button = gtk_button_new_with_label ("Color");
	
	g_signal_connect (G_OBJECT (msg_button), "clicked", G_CALLBACK (msg_button_clicked),
						(gpointer) window);
	g_signal_connect (G_OBJECT (abt_button), "clicked", G_CALLBACK (abt_button_clicked),
						(gpointer) window);
	g_signal_connect (G_OBJECT (file_button), "clicked", G_CALLBACK (file_button_clicked),
						(gpointer) window);
	g_signal_connect (G_OBJECT (choose_folder_button), "clicked", G_CALLBACK (choose_folder_button_clicked),
						(gpointer) window);
	g_signal_connect (G_OBJECT (mulfile_button), "clicked", G_CALLBACK (mulfile_button_clicked),
						(gpointer) window);
	g_signal_connect (G_OBJECT (color_button), "clicked", G_CALLBACK (run_color_dialog),
						(gpointer) window);
																		
	hbox = gtk_hbox_new (TRUE, 5);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), msg_button);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), abt_button);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), file_button);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), choose_folder_button);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), mulfile_button);
	gtk_box_pack_start_defaults (GTK_BOX (hbox), color_button);
	
	gtk_container_add (GTK_CONTAINER (window), hbox);
	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}

static void msg_button_clicked (GtkButton *button, GtkWindow *parent) {
	GtkWidget *dialog;
	dialog = gtk_message_dialog_new (parent, GTK_DIALOG_MODAL, 
										GTK_MESSAGE_INFO, GTK_BUTTONS_OK,
										"Button was clicked!");
	gtk_window_set_title (GTK_WINDOW (dialog), "Information");
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
}

static void abt_button_clicked (GtkButton *button, GtkWindow *parent) {
	GtkWidget *dialog;
	GdkPixbuf *logo;
	GError *error = NULL;
	
	const gchar *authors [] = {
		"Author 1",
		"Author 2",
		NULL
	};
	
	const gchar *documenters [] = {
		"Doc 1",
		"Doc 2",
		NULL
	};
	
	dialog = gtk_about_dialog_new ();
	
	logo = gdk_pixbuf_new_from_file ("/personal/laptop2.png", &error);
	
	if (error == NULL)
		gtk_about_dialog_set_logo (GTK_ABOUT_DIALOG (dialog), logo);
	else {
		if (error->domain == GDK_PIXBUF_ERROR)
			g_print ("GdkPixbufError: %s\n", error->message);
		else if (error->domain == G_FILE_ERROR)
			g_print ("GdkFileError: %s\n", error->message);
		else
			g_print ("Error in domain %d\n", error->domain);
	
		g_error_free (error);
	}
	
	GtkAboutDialog *d = GTK_ABOUT_DIALOG (dialog);
	gtk_about_dialog_set_name (d, "GtkAboutDialog");
	gtk_about_dialog_set_version (d, "1.0");
	gtk_about_dialog_set_copyright (d, "(C) Fred Morcos 2007");
	gtk_about_dialog_set_comments (d, "All About GtkAboutDialog");
	gtk_about_dialog_set_license (d, "Free to all!");
	gtk_about_dialog_set_website (d, "http://fredmorcos.blogspot.com");
	gtk_about_dialog_set_website_label (d, "fredmorcos.blogspot.com");
	gtk_about_dialog_set_authors (d, authors);
	gtk_about_dialog_set_documenters (d, documenters);
	gtk_about_dialog_set_translator_credits (d, "Translator1\nTranslator2");
	
	d = NULL;
	g_free (d);
	gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);

	error = NULL;
}

static void file_button_clicked (GtkButton *button, GtkWindow *window) {
	GtkWidget *dialog;
	gchar *filename;
	
	dialog = gtk_file_chooser_dialog_new ("Save file as...", window,
											GTK_FILE_CHOOSER_ACTION_SAVE,
											GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
											GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
											NULL);
	
	gint result = gtk_dialog_run (GTK_DIALOG (dialog));
	if (result == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_button_set_label (button, filename);
	}
	
	gtk_widget_destroy (dialog);
}

static void choose_folder_button_clicked (GtkButton *button, GtkWindow *window) {
	GtkWidget *dialog;
	gchar *filename;
	
	dialog = gtk_file_chooser_dialog_new ("Save file as...", window,
											GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER,
											GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
											GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
											NULL);
	
	gint result = gtk_dialog_run (GTK_DIALOG (dialog));
	if (result == GTK_RESPONSE_ACCEPT) {
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gtk_button_set_label (button, filename);
	}
	
	gtk_widget_destroy (dialog);
}

static void mulfile_button_clicked (GtkButton *button, GtkWindow *window) {
	GtkWidget *dialog;
	GSList *filenames;
	
	dialog = gtk_file_chooser_dialog_new ("Open File(s)...", window,
											GTK_FILE_CHOOSER_ACTION_OPEN,
											GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
											GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
											NULL);
											
	gtk_file_chooser_set_select_multiple (GTK_FILE_CHOOSER (dialog), TRUE);
	gint result = gtk_dialog_run (GTK_DIALOG (dialog));
	if (result == GTK_RESPONSE_ACCEPT) {
		filenames = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));
		
		gchar *file;
		while (filenames != NULL) {
			file = (gchar *) filenames->data;
			g_print ("%s was selected.\n", file);
			filenames = filenames->next;
		}
	}
	gtk_widget_destroy (dialog);
}

static void run_color_dialog (GtkButton *button, GtkWindow *window) {
	run_color_selection_dialog (button, window, TRUE);
}

static void run_color_selection_dialog (GtkButton *button, GtkWindow *window, gboolean domodal) {
	GtkWidget *dialog, *colorsel;
	gchar *title;
	
	if (domodal)
		title = "Choose Color - Modal";
	else
		title = "Choose Color - Non-Modal";
		
	dialog = gtk_color_selection_dialog_new (title);
	gtk_window_set_modal (GTK_WINDOW (dialog), domodal);
	
	colorsel = GTK_COLOR_SELECTION_DIALOG (dialog) -> colorsel;
	gtk_color_selection_set_has_opacity_control (GTK_COLOR_SELECTION (colorsel), TRUE);
	gtk_color_selection_set_current_color (GTK_COLOR_SELECTION (colorsel), &global_color);
	gtk_color_selection_set_current_alpha (GTK_COLOR_SELECTION (colorsel), global_alpha);
	
	g_signal_connect (G_OBJECT (dialog), "response", G_CALLBACK (dialog_response), NULL);
	
	gtk_widget_show_all (dialog);
}

static void dialog_response (GtkDialog *dialog, gint result, gpointer data) {
	GtkWidget *colorsel;
	GdkColor color = { 0, };
	guint16 alpha = 0;
	
	switch (result) {
		case GTK_RESPONSE_HELP:
			g_print ("Read the Gtk+ API Docs.\n");
			break;
			
		case GTK_RESPONSE_OK:
			colorsel = GTK_COLOR_SELECTION_DIALOG (dialog) -> colorsel;
			alpha = gtk_color_selection_get_current_alpha (GTK_COLOR_SELECTION (colorsel));
			gtk_color_selection_get_current_color (GTK_COLOR_SELECTION (colorsel), &color);
			
			g_print ("#%04X%04X%04X%04X\n", color.red, color.green, color.blue, alpha);
			
			global_color = color;
			global_alpha = alpha;
			
		default:
			gtk_widget_destroy (GTK_WIDGET (dialog));
	}
}
