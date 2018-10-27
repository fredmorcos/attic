#include "vertex.h"
#include <cairo.h>
#include <math.h>
#include <stdlib.h>
#include <glib.h>

Vertex *vertex_new(int ix, int iy, int in)
{
	Vertex *temp = malloc(sizeof(Vertex));
	temp->pPoint = point_new(ix, iy);
	temp->iNum = in;
	return temp;
}

void vertex_expose(Vertex *pV, cairo_t *pC)
{
	cairo_set_source_rgba(pC, 0.0, 0.0, 0.0, 0.5);
	cairo_arc(pC, pV->pPoint->iX, pV->pPoint->iY, VERTEX_RAD, 0, 2 * M_PI);
	cairo_fill(pC);
	cairo_set_source_rgba(pC, 0.0, 0.0, 0.0, 1.0);
	cairo_move_to(pC, pV->pPoint->iX, pV->pPoint->iY);
/*	char tmp[5];
	sprintf(tmp, "%d", pV->iNum);
	cairo_show_text(pC, tmp);
	cairo_fill_preserve(pC);
	cairo_set_source_rgba(pC, 1.0, 1.0, 1.0, 1.0);
	cairo_stroke(pC);
*/
}

void vertex_unload(Vertex *pV)
{
	g_return_if_fail(pV != NULL);
	
	point_unload(pV->pPoint);
	free(pV);
}

gboolean vertex_overlap(Vertex *pV1, Vertex *pV2)
{
	/* distance between two center points */
	return (sqrt(pow(pV2->pPoint->iX - pV1->pPoint->iX, 2) + 
					pow(pV2->pPoint->iY - pV1->pPoint->iY, 2))) 
						< (VERTEX_RAD * 2);
}
