#ifndef EDGE_H
#define EDGE_H

#include "geometry.h"
#include "vertex.h"

#include <stdlib.h>

#define edge_free(x) free(x)

typedef struct {
	Vertex *src,
	       *dst;
	Point  *points;
	int     points_len;
	int     weight;
} Edge;

Edge *edge_new(Vertex *, Vertex *);
void edge_free_deep(Edge *, void (*)(void *));

#endif

