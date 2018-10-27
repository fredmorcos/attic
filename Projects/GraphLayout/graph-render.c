#include "graph-render.h"
#include "cairo-extra.h"
#include "config.h"

void graph_render_to_widget(graph_t *graph, cairo_t *cr, GtkWidget *widget, GdkRectangle rect)
{
  node_t *i;
  vertex_t *v;
  edge_t *e;

  cairo_set_line_width(cr, 1.0);

  if (graph->edges)
    {
      for (i = graph->edges->first; i; i = i->next)
	{
	  e = (edge_t *) i->data;
	  cairo_move_to(cr, e->src->x, e->src->y);
	  cairo_line_to(cr, e->dst->x, e->dst->y);
	  cairo_dark_source(cr);
	  cairo_stroke(cr);
	}
    }

  if (graph->vertices)
    {
      for (i = graph->vertices->first; i; i = i->next)
	{
	  v = (vertex_t *) i->data;

	  if (v->x + NODE_RADI >= rect.x &&
	      v->y + NODE_RADI >= rect.y &&
	      v->x - NODE_RADI <= (rect.x + rect.width) &&
	      v->y - NODE_RADI <= (rect.y + rect.height))
	    {
	      cairo_draw_circle(cr, v->x, v->y, NODE_RADI);
	      cairo_dark_source(cr);
	      cairo_stroke_preserve(cr);
	      cairo_light_source(cr);
	      cairo_fill(cr);
	    }
	}
    }
}
