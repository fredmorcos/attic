#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

#include <gdk/gdkgl.h>

int main (int argc, char *argv[]) {
	GtkWidget *window;
	// GdkGLDrawable *drawable;
	GdkDrawable *drawable;
	GdkGLContext context;
	GdkGLConfig *config;
	
	gtk_init (&argc, &argv);
	gdk_gl_init (&argc, &argv);
	gtk_gl_init (&argc, &argv);
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_size_request (window, 400, 400);
	gtk_container_set_border_width (GTK_CONTAINER (window), 10);
	
	if (gdk_gl_query_extension ()) g_print ("OpenGL Supported.\n");
	else g_print ("No support for OpenGL.\n");
	
	gdk_gl_drawable_make_current (GDK_GL_DRAWABLE (drawable), &context);
	config = gdk_gl_drawable_get_gl_config (GDK_GL_DRAWABLE (drawable));
	
	// gl_working = gtk_widget_set_gl_capabilities (window, config, context, 
	//												FALSE, GDK_GL_RGBA_TYPE);
	
	/* if (gl_working)
		g_print ("Working!\n");
	else
		g_print ("Not working...\n");
	*/
	
	gtk_widget_show_all (window);
	gtk_main ();
	return 0;
}
