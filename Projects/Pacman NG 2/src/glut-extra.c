#include "glut-extra.h"

#include <GL/glut.h>
#include "opengl-render.h"

void initGlutWindow (int iWidth, int iHeight)
{
	glutInitWindowSize (iWidth, iHeight);
	glutInitWindowPosition ((glutGet (GLUT_SCREEN_WIDTH) - iWidth) / 2, 
							(glutGet (GLUT_SCREEN_HEIGHT) - iHeight) / 2);
	glutCreateWindow ("Pacman NG");
}

void initGlut (void (*cbDisplay) (void), void (*cbKeyboard) (unsigned char ucKey, int iX, int iY), void (*cbKeyboardSpecial) (int iKey, int iX, int iY))
{
	glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);	
	glutDisplayFunc (cbDisplay);
//	glutIdleFunc (cbDisplay);
//	glutReshapeFunc (resize);
	glutKeyboardFunc (cbKeyboard);
	glutSpecialFunc (cbKeyboardSpecial);	
}
