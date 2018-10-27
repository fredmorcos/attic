#ifndef VERTEX_H
#define VERTEX_H

#include "geometry.h"

#include <stdlib.h>

#define vertex_free(x) free(x)

typedef struct
{
	Geometry  geometry;
	void     *data;
} Vertex;

Vertex *vertex_new(void *);
void vertex_free_deep(Vertex *, void (*)(void *));

#endif

