#ifndef __GRAPH_H__
#define __GRAPH_H__

#include "vertex.h"
#include "edge.h"
#include <glib.h>
#include <cairo.h>

typedef struct _Graph
{
	GPtrArray *pVList;		/* list that represents vertices */
	GPtrArray *pEList;		/* edges that connect the vertices */
} Graph;

Graph *graph_new();
void graph_unload(Graph *);
void graph_expose(Graph *, cairo_t *);
Vertex *graph_get_vertex(Graph *, int);
Edge *graph_get_edge(Graph *, int);
void graph_add_vertex(Graph *, Vertex *);
void graph_add_edge(Graph *, Edge *);
gboolean graph_vertex_overlap(Graph *);

#endif /* __GRAPH_H__ */
