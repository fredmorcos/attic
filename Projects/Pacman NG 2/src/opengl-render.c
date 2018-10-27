#include "opengl-render.h"
#include "glut-extra.h"

#include <GL/glut.h>

inline void renderOpenGL (int x, int z)
{
	glClear (GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
	gluLookAt (x, 140.0, z + 120.0, x, 0.0, z, 0.0, 1.0, 0.0);
}

void initOpenGL (int iWidth, int iHeight)
{
	glClearColor (1, 1, 1, 0);
	glDisable (GL_DITHER);
	glDisable (GL_NORMALIZE);
	glEnable (GL_COLOR_MATERIAL);
//	glShadeModel(GL_SMOOTH);
//	glShadeModel (GL_FLAT);
	glDisable (GL_TEXTURE_RECTANGLE_ARB);
	glEnable (GL_LIGHTING);
//	glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
//	glEnable (GL_LIGHT0);
//	glEnable (GL_LIGHT1);
//	glEnable (GL_LIGHT2);
	glEnable (GL_LIGHT3);
	glEnable (GL_DEPTH_TEST);
//	glRenderMode (GL_RENDER);
	glDepthFunc (GL_LEQUAL);
	glDepthRange (0, 0.2);
	glDisable (GL_BLEND);
	glDisable (GL_LINE_SMOOTH);
//	glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//	glViewport (0, 0, iWidth, iHeight);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();

	gluPerspective (45.0, ((float) iWidth) / ((float) iHeight), 1.0, 400.0);
	
	/* set the light */
	float light_intensity [] = {1, 1, 1, 0};
	
	float light_position0 [] = {5, 10, 5, 0};
	glLightfv (GL_LIGHT0, GL_POSITION, light_position0);	
	glLightfv (GL_LIGHT0, GL_DIFFUSE, light_intensity);
	
	float light_position1 [] = {-5, 10, 5, 0};
	glLightfv (GL_LIGHT1, GL_POSITION, light_position1);	
	glLightfv (GL_LIGHT1, GL_DIFFUSE, light_intensity);
	
	float light_position2 [] = {-5, 10, -5, 0};
	glLightfv (GL_LIGHT2, GL_POSITION, light_position2);	
	glLightfv (GL_LIGHT2, GL_DIFFUSE, light_intensity);
	
	float light_position3 [] = {0, 0, 10, 0};
	glLightfv (GL_LIGHT3, GL_POSITION, light_position3);	
	glLightfv (GL_LIGHT3, GL_DIFFUSE, light_intensity);
	
	/* set the material color */
	int mat_ambient [] = {1, 1, 1, 1};
	int mat_specular [] = {1, 1, 1, 1};
	int mat_shininess [] = {90};
	glMaterialiv (GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
	glMaterialiv (GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialiv (GL_FRONT, GL_SHININESS, mat_shininess);
}

