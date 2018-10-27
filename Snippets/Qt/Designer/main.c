/*
 * main.c
 *
 *  Created on: Apr 25, 2009
 *      Author: Fred Morcos <fred.morcos@gmail.com>
 */

#include <X11/Xlib.h>
#include <X11/extensions/Xrender.h>
#include <X11/extensions/Xdbe.h>
#include <X11/extensions/Xdamage.h>
#include <cairo/cairo.h>
#include <cairo/cairo-xlib.h>
#include <cairo/cairo-xlib-xrender.h>
#include <stdio.h>
#include <math.h>

typedef cairo_t			Context;
typedef cairo_surface_t Surface;

int main (int argc, char *argv[]) {
	Display				*display;
	Window				window;
	XEvent				event;
	Screen				*screens;
	int					screen;
	Visual				*visual;
	XWindowAttributes	attributes;
	XRenderPictFormat	*format;
	XdbeBackBuffer		buffer;
	XdbeSwapInfo		info;
	Context				*context;
	Surface				*surface;

	display = XOpenDisplay(NULL);
	screen = DefaultScreen(display);
	screens = XScreenOfDisplay(display, screen);
//	window = RootWindow(display, screen);
	window = XCreateSimpleWindow (display, RootWindow (display, screen),
			20, 20, 800, 600, 0, BlackPixel (display, screen), WhitePixel (display, screen));
	visual = DefaultVisual(display, screen);
	format = XRenderFindVisualFormat(display, visual);
	buffer = XdbeAllocateBackBufferName(display, window, XdbeUndefined);

	info.swap_window = window;
	info.swap_action = XdbeUndefined;

	XSelectInput(display, window, ExposureMask);
	XGetWindowAttributes(display, window, &attributes);

	surface = cairo_xlib_surface_create(
		display, buffer, visual, attributes.width, attributes.height);
//	surface = cairo_xlib_surface_create_with_xrender_format(
//		display, buffer, screens, format, attributes.width, attributes.height);
	context = cairo_create(surface);

//	cairo_move_to(context, 20, 20);
//	cairo_set_source_rgb(context, 1.0, 0.0, 0.0);

	XMapWindow (display, window);

	while(1) {
		XNextEvent(display, &event);

		if (event.type == Expose) {
//			cairo_move_to(context, 20, 20);
//			cairo_show_text(context, "Hello!");
//			cairo_fill(context);

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

			XdbeSwapBuffers(display, &info, 1);

			printf("Expose\n");
		}
	}

	cairo_surface_destroy(surface);
	cairo_destroy(context);
	XdbeDeallocateBackBufferName(display, buffer);
	XDestroyWindow(display, window);
	XCloseDisplay(display);

	return 0;
}
