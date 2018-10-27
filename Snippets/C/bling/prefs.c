#include "main.h"
#include "prefs.h"

static gboolean window_delete (GtkWidget *, GdkEvent *, gpointer);

void start_prefs () {
	GtkWidget	*pWindow	= NULL;
	GtkWidget	*pTable		= NULL;
	GtkWidget	*opLabel	= NULL;
	GtkWidget	*opSlider	= NULL;

	GError		*pError		= NULL;

	pWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_icon_from_file (GTK_WINDOW (pWindow),
					"./pixmaps/prefs.svg",
					&pError);
	gtk_widget_set_size_request (pWindow, 400, 400);
	gtk_window_set_resizable (GTK_WINDOW (pWindow), FALSE);
	gtk_widget_set_app_paintable (pWindow, TRUE);
	gtk_window_set_decorated (GTK_WINDOW (pWindow), TRUE);
	gtk_window_set_title (GTK_WINDOW (pWindow), "Bling! Preferences");

	opSlider = gtk_label_new ("Square Opacity");
	opLabel = gtk_label_new ("Boooo");

	pTable = gtk_table_new (5, 3, TRUE);
//	gtk_table_attach_defaults (GTK_TABLE (pTable), opLabel, 0, 0, 0, 0);

	gtk_container_add (GTK_CONTAINER (pWindow), pTable);

	g_signal_connect (G_OBJECT (pWindow),
				"delete-event",
				G_CALLBACK (window_delete),
				NULL);

	gtk_widget_show_all (pWindow);
}

static gboolean window_delete (GtkWidget *widget,
				GdkEvent *event,
	  			gpointer data)
{
	return FALSE;
}
