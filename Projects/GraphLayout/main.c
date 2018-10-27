#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <cairo.h>
#include "graph.h"
#include "graph-gen.h"
#include "graph-render.h"
#include "config.h"

gboolean main_window_delete(GtkWidget *, GdkEvent *, gpointer);
gboolean area_expose(GtkWidget *, GdkEvent *, gpointer);
gboolean area_mapped(GtkWidget *, gpointer);

int
main (int argc, char **argv)
{
  GtkBuilder *builder;
  GtkWidget *window, *area;

  graph_t *graph = graph_new();

  srand(time(NULL));

  gtk_init(&argc, &argv);

  builder = gtk_builder_new();
  gtk_builder_add_from_file(builder, "ui.xml", NULL);

  area = gtk_drawing_area_new();
  /* gtk_widget_add_events(area, GDK_STRUCTURE_MASK); */

  window = GTK_WIDGET(gtk_builder_get_object(builder, "main_window"));
  gtk_container_add(GTK_CONTAINER(window), area);
  gtk_window_maximize(GTK_WINDOW(window));

  g_signal_connect(G_OBJECT(window), "delete-event", G_CALLBACK(main_window_delete), NULL);
  g_signal_connect(G_OBJECT(area), "expose-event", G_CALLBACK(area_expose), (gpointer)graph);
  g_signal_connect(G_OBJECT(area), "realize", G_CALLBACK(area_mapped), (gpointer)graph);

  gtk_widget_show_all(window);
  gtk_main();
  return EXIT_SUCCESS;
}

gboolean
main_window_delete(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
  gtk_widget_destroy(widget);
  gtk_main_quit();
  return FALSE;
}

gboolean
area_expose(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
  graph_t *graph = (graph_t *)user_data;
  cairo_t *cr;
  GdkRectangle rect = ((GdkEventExpose *)event)->area;

  cr = gdk_cairo_create(widget->window);
  cairo_rectangle(cr, rect.x, rect.y, rect.width, rect.height);
  cairo_clip(cr);
  graph_render_to_widget(graph, cr, widget, rect);
  cairo_destroy(cr);
  return TRUE;
}

gboolean
area_mapped(GtkWidget *widget, gpointer user_data)
{
  graph_t *graph = (graph_t *)user_data;
  GtkAllocation allocation;

  gtk_widget_get_allocation(widget, &allocation);

  /* graph_generate_star(graph, NUM_NODES, 0, 0, allocation->width, allocation->height); */
  graph_generate_random_particles(graph, NUM_NODES, 0, 0, allocation.width, allocation.height);

  return TRUE;
}
