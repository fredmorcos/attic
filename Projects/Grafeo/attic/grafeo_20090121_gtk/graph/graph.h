#ifndef GRAPH_H
#define GRAPH_H

#include "vertex.h"
#include "edge.h"

typedef struct {
	Vertex **vertices;
	int      vertices_len;
	Edge   **edges;
	int      edges_len;
} Graph;

Graph *graph_new();
void graph_free(Graph *, void (*)(void *));

#endif

