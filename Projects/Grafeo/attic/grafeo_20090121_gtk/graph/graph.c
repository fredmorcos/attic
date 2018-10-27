#include "graph.h"

#include <stdlib.h>
#include <assert.h>

Graph *graph_new() {
	Graph *tmp;

	tmp = malloc(sizeof(Graph));
	
	return tmp;
}

void graph_free(Graph *graph, void (*vertex_data_dtor)(void *)) {
	assert(graph != NULL);

	for (int i = 0; i < graph->vertices_len; i++)
		vertex_free_deep(graph->vertices[i], vertex_data_dtor);

	for (int i = 0; i < graph->edges_len; i++)
		edge_free_deep(graph->edges[i], vertex_data_dtor);

	free(graph->vertices);
	free(graph->edges);
	free(graph);
}

