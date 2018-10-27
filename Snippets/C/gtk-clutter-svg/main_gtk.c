#include <gtk/gtk.h>
#include <stdlib.h>
#include <librsvg/rsvg.h>

typedef struct _SVGHandle {
	RsvgHandle *handle;
	double scaling;
} SVGHandle;

static gboolean quit (GtkWidget *wid, gpointer data);
static gboolean draw_expose (GtkWidget *wid, GdkEventExpose *event, gpointer data);
static gboolean scale_changed (GtkWidget *wid, gpointer data);

/* we have to fix this!!! */
GtkWidget *draw;
int main (int argc, char *argv[])
{
	GtkWidget *win, *box, *scale;
	GdkPixbuf *pixbuffer;
	SVGHandle *svg = malloc (sizeof (SVGHandle));
	
	gtk_init (&argc, &argv);
	
	win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	box = gtk_vbox_new (FALSE, 5);
	scale = gtk_hscale_new_with_range (1.0, 100.0, 0.5);
	draw = gtk_drawing_area_new ();
	
	svg->handle = rsvg_handle_new_from_file ("heart.svg", NULL);
	svg->scaling = gtk_range_get_value (GTK_RANGE (scale));
	// rsvg_handle_close (handle, NULL);
	// pixbuffer = rsvg_handle_get_pixbuf (handle);
	
	gtk_widget_set_size_request (win, 400, 400);

	// gpointer args [2] = { svg, draw };

	g_signal_connect (G_OBJECT (win), "delete-event", G_CALLBACK (quit), NULL);
	g_signal_connect (G_OBJECT (draw), "expose-event", G_CALLBACK (draw_expose), (gpointer) svg);
	g_signal_connect (G_OBJECT (scale), "value-changed", G_CALLBACK (scale_changed), (gpointer) svg);
	
	gtk_container_add (GTK_CONTAINER (win), box);
	gtk_box_pack_start (GTK_BOX (box), draw, TRUE, TRUE, 5);
	gtk_box_pack_start (GTK_BOX (box), scale, FALSE, FALSE, 5);
	
	gtk_widget_show_all (win);
	gtk_main ();
	
	rsvg_handle_free (svg->handle);
	free (svg);

	return 0;
}

static gboolean scale_changed (GtkWidget *wid, gpointer data)
{
	((SVGHandle *) data)->scaling = gtk_range_get_value (GTK_RANGE (wid));
	
	// data += sizeof (SVGHandle *);
	gtk_widget_queue_draw (draw);
}

static gboolean draw_expose (GtkWidget *wid, GdkEventExpose *event, gpointer data)
{
	// GdkPixbuf *pic_file = rsvg_pixbuf_from_file ("heart.svg", NULL);
	// GdkPixbuf *pic_file = rsvg_pixbuf_from_file_at_zoom ("heart.svg", 1.0, 1.0, NULL);
	
	cairo_t *cr = gdk_cairo_create (wid->window);

//	cairo_rectangle (cr, event->area.x, event->area.y, event->area.width, event->area.height);
//	cairo_clip (cr);

	/* 5x WE DO NOT LOSE RESOLUTION!!! YAAAY!!! */
	cairo_scale (cr, ((SVGHandle *) data)->scaling, ((SVGHandle *) data)->scaling);
	
	rsvg_handle_render_cairo (((SVGHandle *) data)->handle, cr);
	
	/* scale back to 1x */
	cairo_scale (cr, (1.0)/((SVGHandle *) data)->scaling, (1.0)/((SVGHandle *) data)->scaling);
	
	/* REMOVE THE CAIRO STUFF WHEN NOT NEEDED, USED TO TEST IF CAIRO WORKS WELL WITH SVG RENDERING */
	cairo_move_to (cr, 0, 0);
	cairo_line_to (cr, 35, 0);
	cairo_line_to (cr, 50, 50);
	cairo_close_path (cr);
	cairo_set_source_rgba (cr, 0.2, 0.2, 0.2, 0.5);
	cairo_fill (cr);
	cairo_move_to (cr, 50, 50);
	
	// gdk_draw_pixbuf (wid->window, NULL, (GdkPixbuf *) data, 0, 0, 0, 0, -1, -1, GDK_RGB_DITHER_NORMAL, 1, 1);
}

static gboolean quit (GtkWidget *wid, gpointer data)
{
	gtk_main_quit ();
}

