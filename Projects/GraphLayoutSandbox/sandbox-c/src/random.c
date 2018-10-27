#include "random.h"
#include "graph.h"
#include "vertex.h"
#include <glib.h>
#include <math.h>

void fix_overlap (Graph *, int, int);

void arrange_random(Graph *pG, int area_w, int area_h)
{
	int i = 0;
	GRand *pRand = g_rand_new();
	
	while (i < pG->pVList->len)
	{
		(graph_get_vertex(pG, i))->pPoint->iX = 
					g_rand_int_range(pRand, VERTEX_RAD, area_w - VERTEX_RAD);
		(graph_get_vertex(pG, i))->pPoint->iY = 
					g_rand_int_range(pRand, VERTEX_RAD, area_h - VERTEX_RAD);
		i++;
	}
	
	fix_overlap(pG, area_w, area_h);
	
	g_rand_free(pRand);
}

void fix_overlap (Graph *pG, int area_w, int area_h)
{
	if ((pow((VERTEX_RAD * 2), 2) * pG->pVList->len) > (area_w * area_h))
	{
		g_print("Area too small to avoid overlaps.\n");
		return;
	}

	int i = 0, j = 0;
	GRand *pRand = g_rand_new();
	
	while (i < pG->pVList->len)
	{
		j = 0;
		while (j < pG->pVList->len)
		{
			while ((i != j) && (vertex_overlap(
									graph_get_vertex(pG, i),
									graph_get_vertex(pG, j)) == TRUE))
			{
				(graph_get_vertex(pG, j))->pPoint->iX = 
					g_rand_int_range(pRand, VERTEX_RAD, area_w - VERTEX_RAD);
				(graph_get_vertex(pG, j))->pPoint->iY = 
					g_rand_int_range(pRand, VERTEX_RAD, area_h - VERTEX_RAD);
					
				if (gtk_events_pending())
					gtk_main_iteration_do(FALSE);
				
				j--;
				i = 0;
				break;
			}
			j++;
		}
		i++;
	}
	
	g_rand_free(pRand);
}
