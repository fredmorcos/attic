#include "table.h"
#include "graph.h"
#include "vertex.h"

void arrange_table(Graph *pG, int area_w, int area_h)
{
	int max_cols = (area_w - VERTEX_RAD) / (VERTEX_RAD * 2);
	int i = 0;
	
	while (i < pG->pVList->len)
	{
		(graph_get_vertex(pG, i))->pPoint->iX = 
			((i - ((i / max_cols) * max_cols)) * (VERTEX_RAD * 2)) + VERTEX_RAD;
		(graph_get_vertex(pG, i))->pPoint->iY = 
			((i / max_cols) * (VERTEX_RAD * 2)) + VERTEX_RAD;
		i++;
	}
}
