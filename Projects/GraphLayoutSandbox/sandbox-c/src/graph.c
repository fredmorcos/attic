#include "graph.h"
#include "edge.h"
#include "vertex.h"
#include <glib.h>
#include <stdlib.h>

Graph *graph_new()
{
	Graph *temp = malloc(sizeof(Graph));
	temp->pVList = g_ptr_array_new();
	temp->pEList = g_ptr_array_new();
	return temp;
}

void graph_unload(Graph *pG)
{
	g_return_if_fail(pG != NULL);
	
	int i = 0;
	while (i < pG->pVList->len)
	{
		vertex_unload(graph_get_vertex(pG, i));
		i++;
	}
	
	i = 0;
	while (i < pG->pEList->len)
	{
		edge_unload(graph_get_edge(pG, i));
		i++;
	}
	
	g_ptr_array_free(pG->pVList, TRUE);
	g_ptr_array_free(pG->pEList, TRUE);
}

void graph_expose(Graph *pG, cairo_t *pC)
{
	int i = 0;
	while (i < pG->pVList->len)
	{
		vertex_expose(graph_get_vertex(pG, i), pC);
		i++;
	}
	
/*	i = 0;
	while (i < pG->pEList->len)
	{
		edge_expose(graph_get_edge(pG, i), pC);
		i++;
	}
*/
}

Vertex *graph_get_vertex(Graph *pG, int i)
{
	return (Vertex *)g_ptr_array_index(pG->pVList, i);
}

Edge *graph_get_edge(Graph *pG, int i)
{
	return (Edge *)g_ptr_array_index(pG->pEList, i);
}

void graph_add_vertex(Graph *pG, Vertex *pV)
{
	g_ptr_array_add(pG->pVList, (gpointer) pV);
}

void graph_add_edge(Graph *pG, Edge *pE)
{
	g_ptr_array_add(pG->pEList, (gpointer) pE);
}

gboolean graph_vertex_overlap(Graph *pG)
{
	int i = 0, j = 0;
	while (i < pG->pVList->len)
	{
		j = 0;
		while (j < pG->pVList->len)
		{
			if ((i != j) && (vertex_overlap(
								graph_get_vertex(pG, i),
								graph_get_vertex(pG, j)) == TRUE))
				return TRUE;
			j++;
		}
		i++;
	}
	
	return FALSE;
}
