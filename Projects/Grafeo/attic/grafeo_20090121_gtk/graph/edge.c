#include "edge.h"
#include "vertex.h"

#include <assert.h>

Edge *edge_new(Vertex *src, Vertex *dst) {
	Edge *tmp;

	tmp = malloc(sizeof(Edge));
	tmp->src = src;
	tmp->dst = dst;
	tmp->points = NULL;
	tmp->points_len = 0;
	tmp->weight = 0;

	return tmp;
}

void edge_free_deep(Edge *edge, void (*vertex_data_dtor)(void *)) {
	assert(edge != NULL);

	vertex_free_deep(edge->src, vertex_data_dtor);
	vertex_free_deep(edge->dst, vertex_data_dtor);

	free(edge->points);
	edge_free(edge);
}

