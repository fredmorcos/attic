#include "edge.h"
#include "vertex.h"
#include <stdlib.h>
#include <cairo.h>
#include <glib.h>

Edge *edge_new(Vertex *pva, Vertex *pvb)
{
	Edge *temp = malloc(sizeof(Edge));
	temp->pVA = pva;
	temp->pVB = pvb;
	return temp;
}

void edge_expose(Edge *pE, cairo_t *pC)
{
	cairo_set_source_rgba(pC, 0.0, 0.0, 0.0, 0.5);
	cairo_set_line_width(pC, 0.5);
	cairo_move_to(pC, pE->pVA->pPoint->iX, pE->pVA->pPoint->iY);
	cairo_line_to(pC, pE->pVB->pPoint->iX, pE->pVB->pPoint->iY);
	cairo_stroke(pC);
}

void edge_unload(Edge *pE)
{
	g_return_if_fail(pE != NULL);
	
	free(pE);
}
