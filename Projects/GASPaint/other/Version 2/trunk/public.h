#ifndef _PUBLIC_H_
#define _PUBLIC_H_

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

/* declares some public stuff */

#define LINE 0
#define RECT 1
#define CIRC 2
#define POLY 3
#define FILL 4

GtkWidget *main_window;

GtkWidget *draw_area;
GdkGLConfig *draw_config;

GdkColor current_color;

gint current_tool;
gint current_x0, current_y0, current_x1, current_y1;
gint num_clicks;

GString *events;

#endif
