#include <gtk/gtk.h>
#include <gdk/gdkscreen.h>
#include <cairo.h>

/*
 * This program shows you how to create semi-transparent windows,
 * without any of the historical screenshot hacks. It requires
 * a modern system, with a compositing manager. I use xcompmgr
 * and the nvidia drivers with RenderAccel, and it works well.
 *
 * I'll take you through each step as we go. Minimal GTK+ knowledge is
 * assumed.
 *
 * Original code by: Mike - http://plan99.net/~mike/blog
 * Extended with slidebar by: Fred Morcos - http://fredmorcos.blogspot.com/
 */

/* function prototypes */
static void screen_changed(GtkWidget *widget, GdkScreen *old_screen, gpointer user_data);
static gboolean expose(GtkWidget *widget, GdkEventExpose *event, gpointer user_data);
static void clicked(GtkWindow *win, GdkEventButton *event, gpointer user_data);
static gboolean alphademo_slider_value_changed(GtkRange *range, GtkScrollType scroll, gdouble value, gpointer user_data);

/* our main window, have it public so we can call gtk_widget_queue_draw(),
 * for some reason gtk_widget_get_parent_window() isn't working as expected
 */
GtkWidget *window;
/* the alpha value of the circle */
double alpha_value = 0.8;

int main(int argc, char **argv)
{
    /* boilerplate initialization code */
    gtk_init(&argc, &argv);
    
    /* the slider that will set alpha_value 
     * and emit "change-value" to repaint the new circle.
     */
    GtkWidget *slider = gtk_hscale_new_with_range(10.0, 100.0, 5.0);
    /* gtk_range_set_update_policy(GTK_RANGE(slider), GTK_UPDATE_CONTINUOUS); */
    gtk_range_set_update_policy(GTK_RANGE(slider), GTK_UPDATE_DELAYED);
    gtk_range_set_value(GTK_RANGE(slider), alpha_value*100.0);
	g_signal_connect(G_OBJECT(slider), "change-value", G_CALLBACK(alphademo_slider_value_changed), NULL);
	
	/* a table to add the slider to */
	GtkWidget *table = gtk_table_new(2, 1, FALSE);
	gtk_table_attach(GTK_TABLE(table), GTK_WIDGET(slider), 0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
	
	/* some settings of our main window */
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Alpha Demo");
	gtk_window_resize(GTK_WINDOW(window), 200, 200);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
    gtk_container_add(GTK_CONTAINER(window), table);
    g_signal_connect(G_OBJECT(window), "delete-event", gtk_main_quit, NULL);

    /* Tell GTK+ that we want to draw the windows background ourself.
     * If we don't do this then GTK+ will clear the window to the
     * opaque theme default color, which isn't what we want.
     */
    gtk_widget_set_app_paintable(window, TRUE);

    /* We need to handle two events ourself: "expose-event" and "screen-changed".
     *
     * The X server sends us an expose event when the window becomes
     * visible on screen. It means we need to draw the contents.  On a
     * composited desktop expose is normally only sent when the window
     * is put on the screen. On a non-composited desktop it can be
     * sent whenever the window is uncovered by another.
     *
     * The screen-changed event means the display to which we are
     * drawing changed. GTK+ supports migration of running
     * applications between X servers, which might not support the
     * same features, so we need to check each time.
     */

    g_signal_connect(G_OBJECT(window), "expose-event", G_CALLBACK(expose), NULL);
    g_signal_connect(G_OBJECT(window), "screen-changed", G_CALLBACK(screen_changed), NULL);

    /* toggle title bar on click - we add the mask to tell X we are interested in this event */
    gtk_window_set_decorated(GTK_WINDOW(window), FALSE);
    gtk_widget_add_events(window, GDK_BUTTON_PRESS_MASK);range : 
    g_signal_connect(G_OBJECT(window), "button-press-event", G_CALLBACK(clicked), NULL);

    /* initialize for the current display */
    screen_changed(window, NULL, NULL);

    /* Run the program */
    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

/* called whenever the slider it moved,
 * in turn calls gtk_widget_queue_draw() to redraw the window with the new circle
 */
static gboolean alphademo_slider_value_changed(GtkRange *range, GtkScrollType scroll, gdouble value, gpointer user_data)
{
	alpha_value = value / 100.0;
	gtk_widget_queue_draw(window);
	return FALSE;
}

/* Only some X servers support alpha channels. Always have a fallback */
gboolean supports_alpha = FALSE;

static void screen_changed(GtkWidget *widget, GdkScreen *old_screen, gpointer userdata)
{
    /* To check if the display supports alpha channels, get the colormap */
    GdkScreen *screen = gtk_widget_get_screen(widget);
    GdkColormap *colormap = gdk_screen_get_rgba_colormap(screen);

    if (!colormap)
    {
        printf("Your screen does not support alpha channels!\n");
        colormap = gdk_screen_get_rgb_colormap(screen);
        supports_alpha = FALSE;
    }
    else
    {
        printf("Your screen supports alpha channels!\n");
        supports_alpha = TRUE;
    }

    /* Now we have a colormap appropriate for the screen, use it */
    gtk_widget_set_colormap(widget, colormap);
}

/* This is called when we need to draw the windows contents,
 * also called when the slider is moved.
 */
static gboolean expose(GtkWidget *widget, GdkEventExpose *event, gpointer userdata)
{
    cairo_t *cr = gdk_cairo_create(widget->window);

    if (supports_alpha) {
        cairo_set_source_rgba (cr, 1.0, 1.0, 1.0, 0.0); /* transparent */
    }
    else {
        cairo_set_source_rgb (cr, 1.0, 1.0, 1.0); /* opaque white */
    }

    /* draw the background */
    cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
    cairo_paint (cr);

    /* draw a circle */
    int width, height;
    gtk_window_get_size(GTK_WINDOW(widget), &width, &height);

    cairo_set_source_rgba(cr, 1, 0.2, 0.2, alpha_value);
    cairo_arc(cr, width / 2, height / 2, (width < height ? width : height) / 2 - 8 , 0, 2 * 3.14);
    cairo_fill(cr);
    cairo_stroke(cr);
    
    cairo_destroy(cr);
    return FALSE;
}

static void clicked(GtkWindow *win, GdkEventButton *event, gpointer user_data)
{
    /* toggle window manager frames */
    gtk_window_set_decorated(win, !gtk_window_get_decorated(win));
}

