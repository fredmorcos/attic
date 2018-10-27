#include <GL/glut.h>
#include <stdlib.h>

#include "global.h"
#include "man.h"

man *newMan (float ix, float iy, float iz, boolean be)
{
	man *temp = calloc (1, sizeof (man));
	temp->ix = ix;
	temp->iy = iy;
	temp->iz = iz;
	temp->bEvil = be;
	
	return temp;
}

inline void renderMan (man *m)
{
	glPushMatrix ();
	if (m->bEvil)
		glColor3d (1, 0, 0);
	else
		glColor3d (1, 1, 0);
	
	glTranslatef (m->ix * CELL_SIZE, m->iy + (CELL_SIZE / 2), m->iz * CELL_SIZE);
	glutSolidSphere (CELL_SIZE / 2, 50, 50);
	glPopMatrix ();
}
