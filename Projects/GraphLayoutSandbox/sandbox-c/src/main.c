#include <gtk/gtk.h>
#include <glib.h>
#include <cairo.h>
#include <stdlib.h>
#include "graph.h"
#include "table.h"
#include "random.h"

#define GRAPH_SIZE 1000

static gboolean area_expose(GtkWidget *, GdkEventExpose *, gpointer);
static void button_random_clicked(GtkButton *, gpointer);
static void button_table_clicked(GtkButton *, gpointer);

Graph *graph;
cairo_t *pCairo;

int main(int argc, char* argv[])
{
	GtkWidget *pWindow;
	GtkWidget *pBoxVertical;
	GtkWidget *pBoxHorizontal;
	GtkWidget *pDrawArea;
	GtkWidget *pButtonRandom;
	GtkWidget *pButtonTable;
	
	GRand *pRand;
	int i = 0;
	
	gtk_init(&argc, &argv);
	gtk_init(&argc, &argv);

	pWindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_container_set_border_width(GTK_CONTAINER(pWindow), 2);
	gtk_window_set_default_size(GTK_WINDOW(pWindow), 300, 300);
	
	pDrawArea = gtk_drawing_area_new();
	gtk_widget_set_size_request(pDrawArea, 100, 100);
	
	pButtonRandom = gtk_button_new_with_label("Random");
	pButtonTable = gtk_button_new_with_label("Table");
	
	pBoxVertical = gtk_vbox_new(FALSE, 2);
	pBoxHorizontal = gtk_hbox_new(TRUE, 2);
	
	gtk_container_add(GTK_CONTAINER(pWindow), pBoxVertical);
	gtk_box_pack_start(GTK_BOX(pBoxVertical), pDrawArea, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pBoxVertical), pBoxHorizontal, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pBoxHorizontal), pButtonRandom, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pBoxHorizontal), pButtonTable, TRUE, TRUE, 0);

	graph = graph_new();
	
	i = 0;
	while (i < GRAPH_SIZE)
	{
		graph_add_vertex(graph, vertex_new(VERTEX_RAD, VERTEX_RAD, i));
		i++;
	}
	
	pRand =  g_rand_new();
	
	i = 0;
	while (i < GRAPH_SIZE)
	{
		graph_add_edge(graph, 
			edge_new(
				graph_get_vertex(graph, 
					g_rand_int_range(pRand, 0, GRAPH_SIZE - 1)), 
				graph_get_vertex(graph, i)));
		i++;
	}
	
	g_rand_free(pRand);

	g_signal_connect(G_OBJECT(pWindow), "destroy", 
						G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(pDrawArea), "expose-event",
						G_CALLBACK(area_expose), NULL);
	g_signal_connect(G_OBJECT(pButtonRandom), "clicked",
						G_CALLBACK(button_random_clicked), pDrawArea);
	g_signal_connect(G_OBJECT(pButtonTable), "clicked",
						G_CALLBACK(button_table_clicked), pDrawArea);
												
	gtk_widget_show_all(pWindow);
	gtk_main();
	
	graph_unload(graph);
	g_free(pWindow);
	
	return 0;
}

static gboolean area_expose(GtkWidget *pWidget, 
							GdkEventExpose *pEvent,
							gpointer gpData)
{
	pCairo = gdk_cairo_create(pWidget->window);
	
	/* clip around the draw area to make things faster */
	cairo_rectangle(pCairo, 0.0, 0.0, 
							pWidget->allocation.width, 
							pWidget->allocation.height);
	cairo_clip(pCairo);
	
	graph_expose(graph, pCairo);
	
	cairo_destroy(pCairo);
	return FALSE;
}

static void button_random_clicked(GtkButton *pButton, gpointer gpData)
{
	arrange_random(graph,
					((GtkWidget *)gpData)->allocation.width, 
					((GtkWidget *)gpData)->allocation.height);
	gtk_widget_queue_draw(gpData);
}

static void button_table_clicked(GtkButton *pButton, gpointer gpData)
{
	arrange_table(graph,
					((GtkWidget *)gpData)->allocation.width, 
					((GtkWidget *)gpData)->allocation.height);
	gtk_widget_queue_draw(gpData);
}
