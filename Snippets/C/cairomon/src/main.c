#include <gtk/gtk.h>
// #include <glibtop/sysinfo.h>
#include <math.h>

#define SIZE_PERCENT 20 / 100

static void screen_changed (GtkWidget *widget, GdkScreen *previous_screen, gpointer user_data);
static gboolean expose_event (GtkWidget *widget, GdkEventExpose *event, gpointer user_data);
void g_hash_table_print (gpointer key, gpointer value, gpointer user_data);

gboolean composited = FALSE, alpha = FALSE;
gint screen_w = 0, screen_h = 0;

int main (int argc, char *argv [])
{
	GtkWidget *window, *label;
	char *markup;
	
	gtk_init (&argc, &argv);
	
	// int i = 0;
	// const glibtop_sysinfo *info = glibtop_get_sysinfo ();
	// glibtop_cpu cpu;
	// while (i < info->ncpu)
	// {
	//	printf ("CPU %d:\n\t%s\n\t%s\n", i, 
	//			(char *)g_hash_table_lookup (info->cpuinfo [i].values, "model name"),
	//			(char *)g_hash_table_lookup (info->cpuinfo [i].values, "frequency")
	//			);
	//	i++;
	// }
	
	// g_hash_table_foreach (info->cpuinfo [0].values, g_hash_table_print, NULL);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_double_buffered (window, TRUE);
	gtk_widget_set_app_paintable (window, TRUE);
	gtk_window_set_decorated (GTK_WINDOW (window), FALSE);
	gtk_window_set_title (GTK_WINDOW (window), "cairomon");
	// gtk_window_set_type_hint (GTK_WINDOW (window), GDK_WINDOW_TYPE_HINT_DESKTOP);
	gtk_window_set_skip_taskbar_hint (GTK_WINDOW (window), TRUE);
	gtk_window_set_skip_pager_hint (GTK_WINDOW (window), TRUE);
	gtk_window_set_policy (GTK_WINDOW (window), FALSE, FALSE, FALSE);
	
	g_signal_connect (G_OBJECT (window), "expose-event", G_CALLBACK (expose_event), NULL);
	g_signal_connect (G_OBJECT (window), "screen-changed", G_CALLBACK (screen_changed), NULL);

	label = gtk_label_new ("");
	markup = g_markup_printf_escaped ("<span foreground=\"#FFFFFF\"><b>%s</b></span>", "Cairo System Monitor");
	gtk_label_set_markup (GTK_LABEL (label), markup);
	g_free (markup);

	gtk_container_add (GTK_CONTAINER (window), label);
	
	screen_changed (window, NULL, NULL);
	
	gtk_widget_show_all (window);
	gtk_main ();

	g_free (window);
	g_free (label);
	
	return 0;
}

void g_hash_table_print (gpointer key, gpointer value, gpointer user_data)
{
	printf ("Key: %s\nValue: %s\n", (char *) key, (char *) value);
}

static void screen_changed (GtkWidget *widget, GdkScreen *previous_screen, gpointer user_data)
{
	GdkScreen *screen = gtk_widget_get_screen (widget);
	GdkColormap *colormap = gdk_screen_get_rgba_colormap (screen);
	screen_w = gdk_screen_get_width (screen);
	screen_h = gdk_screen_get_height (screen);
	composited = gdk_screen_is_composited (screen);
	
	if (!colormap)
	{
		colormap = gdk_screen_get_rgb_colormap (screen);
		alpha = FALSE;
	}
	else
		alpha = TRUE;
	
	g_print ("Compositor is running: %d\n", composited);
	g_print ("Alpha support: %d\n", alpha);
	
	gtk_widget_set_colormap (widget, colormap);
	
	gtk_widget_set_size_request (widget, screen_w * SIZE_PERCENT, screen_h * SIZE_PERCENT);
}

gboolean expose_event (GtkWidget *widget, GdkEventExpose *event, gpointer user_data)
{
	cairo_t *cr = gdk_cairo_create (widget->window);
	gint width, height;
	
	gtk_window_get_size (GTK_WINDOW (widget), &width, &height);
	
	cairo_set_line_width (cr, 10.0);
	if (alpha)
	{
		/* draw a rounded rectangle and use alpha */
		cairo_set_source_rgba (cr, 0.2, 0.2, 0.2, 0.0);
		cairo_rectangle (cr, 0.0, 0.0, width, height);
		cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
		cairo_fill (cr);
		
		cairo_set_source_rgba (cr, 0.1, 0.1, 0.1, 0.8);
		cairo_set_line_width (cr, 1.0);
		cairo_move_to (cr, 20.0, 0.0);
		cairo_line_to (cr, width - 20.0, 0.0);
		cairo_curve_to (cr, width - 20.0, 0.0, width, 0.0, width, 20.0);
		cairo_line_to (cr, width, height - 20.0);
		cairo_curve_to (cr, width, height - 20.0, width, height, width - 20.0, height);
		cairo_line_to (cr, 20.0, height);
		cairo_curve_to (cr, 20.0, height, 0.0, height, 0.0, height - 20.0);
		cairo_line_to (cr, 0.0, 20.0);
		cairo_curve_to (cr, 0.0, 20.0, 0.0, 0.0, 20.0, 0.0);
		cairo_fill (cr);

#define MARGIN 20.0
		cairo_set_line_width (cr, 1.0);
		cairo_set_source_rgba (cr, 1.0, 1.0, 1.0, 0.8);
		cairo_move_to (cr, MARGIN, 0.0);
		cairo_line_to (cr, width - MARGIN, 0.0);
		cairo_curve_to (cr, width - MARGIN, 0.0, width, 0.0, width, MARGIN);
		cairo_line_to (cr, width, height - MARGIN);
		cairo_curve_to (cr, width, height - MARGIN, width, height, width - MARGIN, height);
		cairo_line_to (cr, MARGIN, height);
		cairo_curve_to (cr, MARGIN, height, 0.0, height, 0.0, height - MARGIN);
		cairo_line_to (cr, 0.0, MARGIN);
		cairo_curve_to (cr, 0.0, MARGIN, 0.0, 0.0, MARGIN, 0.0);
		cairo_stroke (cr);
	}
	else
	{
		/* draw a normal rectangle and don't use alpha */
		cairo_set_source_rgb (cr, 1.0, 1.0, 1.0);
	}
	
	return FALSE;
}
