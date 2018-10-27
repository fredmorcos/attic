#ifndef __GRAPH_RENDER_H__
#define __GRAPH_RENDER_H__

#include <gtk/gtk.h>
#include <cairo.h>
#include "graph.h"

void graph_render_to_widget(graph_t *, cairo_t *, GtkWidget *, GdkRectangle);

#endif /* __GRAPH_RENDER_H__ */
