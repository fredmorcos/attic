#ifndef __VERTEX_H__
#define __VERTEX_H__

#include "point.h"
#include <glib.h>
#include <cairo.h>

#define VERTEX_RAD 5

typedef struct _Vertex
{
	Point *pPoint;
	int iNum;
} Vertex;

Vertex *vertex_new(int, int, int);
void vertex_expose(Vertex *, cairo_t *);
void vertex_unload(Vertex *);
gboolean vertex_overlap(Vertex *, Vertex *);

#endif /* __VERTEX_H__ */
