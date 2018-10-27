#ifndef __EDGE_H__
#define __EDGE_H__

#include "vertex.h"
#include <cairo.h>

typedef struct _Edge
{
	Vertex *pVA, *pVB;
} Edge;

Edge *edge_new(Vertex *, Vertex *);
void edge_expose(Edge *, cairo_t *);
void edge_unload(Edge *);

#endif /* __EDGE_H__ */
