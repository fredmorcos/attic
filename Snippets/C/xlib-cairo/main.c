#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <cairo/cairo.h>
#include <cairo/cairo-xlib.h>

int main()
{
	Display *display;
	Window window;
	XEvent event;
	XWindowAttributes attributes;
	int screen;
	cairo_t *context;
	cairo_surface_t *surface;
	
	if ((display = XOpenDisplay (NULL)) == NULL)
		printf ("Cannot open display.\n");
	
	screen = DefaultScreen (display);
	window = XCreateSimpleWindow (display, RootWindow (display, screen),
		20, 20, 800, 600, 0, BlackPixel (display, screen), WhitePixel (display, screen));

//	window = RootWindow (display, screen);

	XSelectInput (display, window, ExposureMask | KeyPressMask);
	surface = cairo_xlib_surface_create (display, window, DefaultVisual (display, screen), 0, 0);
	context = cairo_create (surface);
	XMapWindow (display, window);
	
	while (1)
	{
		XNextEvent (display, &event);
		
		if (event.type == Expose)
		{
			XGetWindowAttributes (display, window, &attributes);
			cairo_xlib_surface_set_size (surface, attributes.width, attributes.height);

//			cairo_rectangle (context, 0, 0, attributes.width, attributes.height);
//			cairo_clip (context);

//			cairo_push_group(context);			
			cairo_set_line_width (context, 1.0);
			cairo_set_source_rgba (context, 1.0, 0.0, 0.0, 1.0);
			cairo_arc (context, attributes.width / 2, attributes.height / 2,
					   attributes.height / 2 - 10, 0, 2 * M_PI);
			cairo_stroke_preserve (context);
			cairo_set_source_rgba (context, 0.0, 1.0, 0.0, 1.0);
			cairo_fill (context);
			
			cairo_set_source_rgba (context, 0.0, 0.0, 1.0, 0.3);
			cairo_set_line_width (context, 6);
			cairo_arc (context, attributes.width, attributes.height / 2,
					   attributes.height / 2 - 10, 0, 2 * M_PI);
			cairo_stroke_preserve (context);
			cairo_set_source_rgba (context, 1.0, 0.0, 0.0, 0.3);
			cairo_fill (context);
			
			cairo_set_source_rgba (context, 0.2, 1.0, 0.2, 1.0);
			cairo_set_line_width (context, 8);
			cairo_set_line_join (context, CAIRO_LINE_JOIN_ROUND);
			cairo_rectangle (context, 10, 10, 40, 40);
			cairo_stroke_preserve (context);
			cairo_set_source_rgba (context, 0.6, 1.0, 0.6, 1.0);
			cairo_fill (context);
//			cairo_pop_group_to_source(context);
		}
		else if (event.type == KeyPress)
			break;
	}
	
	cairo_surface_destroy (surface);
	cairo_destroy (context);
	XDestroyWindow (display, window);
	XCloseDisplay (display);
	return 0;
}
