#include "point.h"
#include <stdlib.h>
#include <glib.h>

Point *point_new(int ix, int iy)
{
	Point *temp = malloc(sizeof(Point));
	temp->iX = ix;
	temp->iY = iy;	
	return temp;
}

void point_unload(Point *pP)
{
	g_return_if_fail(pP != NULL);
	
	free(pP);
}
