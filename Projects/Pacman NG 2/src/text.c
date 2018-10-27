#include <GL/glut.h>

#include "text.h"

void renderText (char *x)
{
	glColor3d (1, 0, 0);
	glRasterPos3d (0, 0, -100);
	while ((*x) != '\0')
		glutBitmapCharacter (GLUT_BITMAP_9_BY_15, *x++);
}
