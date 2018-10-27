#include "main.h"
#include "bling.h"

static gboolean window_delete (GtkWidget *, GdkEvent *, gpointer);
static gboolean window_expose (GtkWidget *, GdkEventExpose *, gpointer);

void start_bling () {
	GtkWidget	*mWindow	= NULL;
	GError		*mError		= NULL;

	mWindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_icon_from_file (GTK_WINDOW (mWindow),
					"./pixmaps/icon.svg",
					&mError);
	gtk_widget_set_size_request (mWindow, 400, 400);
	gtk_window_set_resizable (GTK_WINDOW (mWindow), FALSE);
	//gtk_widget_set_app_paintable (mWindow, TRUE);
	gtk_window_set_decorated (GTK_WINDOW (mWindow), TRUE);
	gtk_window_set_title (GTK_WINDOW (mWindow), "Bling!");
	gtk_widget_set_double_buffered (mWindow, TRUE);

	g_signal_connect (G_OBJECT (mWindow),
				"delete-event",
    				G_CALLBACK (window_delete),
				NULL);
	g_signal_connect (G_OBJECT (mWindow),
				"expose-event",
    				G_CALLBACK (window_expose),
				NULL);

	gtk_widget_show_all (mWindow);
}

static gboolean window_delete (GtkWidget *widget,
				GdkEvent *event,
				gpointer data)
{
	gtk_main_quit ();
	return FALSE;
}

static gboolean window_expose (GtkWidget *widget,
				GdkEventExpose *event,
				gpointer data)
{
	static cairo_t		*cr		= NULL;
	static cairo_surface_t	*bgImage	= NULL;

	cr = gdk_cairo_create (widget->window);

	cairo_rectangle (cr, event->area.x, event->area.y, event->area.width, event->area.height);
	cairo_clip (cr);

	bgImage = cairo_image_surface_create_from_png ("./pixmaps/bg.png");

	cairo_set_source_surface (cr, bgImage, 0, 0);
	cairo_paint (cr);
	cairo_surface_destroy (bgImage);

	cairo_move_to (cr, 50, 50);
	cairo_line_to (cr, 350, 50);
	cairo_line_to (cr, 350, 350);
	cairo_line_to (cr, 50, 350);
	cairo_line_to (cr, 50, 50);
	cairo_close_path (cr);
	cairo_set_source_rgba (cr, 0.2, 0.2, 0.2, 0.5);
	cairo_fill (cr);

	cairo_select_font_face (cr,
				"Sans",
				CAIRO_FONT_SLANT_NORMAL,
				CAIRO_FONT_WEIGHT_BOLD);
	cairo_set_font_size (cr, 48);
	cairo_move_to (cr, 60, 60);
	cairo_text_path (cr, "Bling!");
	cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.5);
	cairo_fill_preserve (cr);
	cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 1.0);
	cairo_stroke (cr);

	return TRUE;
}
