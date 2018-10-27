#include "main.h"

static void glInit (void);
static void glDisplay (void);

int main (int argc, char *argv[])
{
	init_ui ();

	glutInit (&argc, argv);
	glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB);
	glutInitWindowSize (WIN_W, WIN_H);
	glutInitWindowPosition (WIN_X, WIN_Y);
	glutCreateWindow ("Gas");
	glutDisplayFunc (glDisplay);
	glInit ();
	
	glutMouseFunc (processMouse);
	glutPassiveMotionFunc (processMousePassiveMotion);
	glutMotionFunc (processMousePassiveMotion); // makes the x and y print even when the mouse is pressed like in dragging
	fileMenu ();	/* calls the file menu */
	glutMenuStatusFunc (processMenuStatus);

	glutMainLoop ();

	return 0;
}

static void glInit ()
{
	glClearColor (1.0, 1.0, 1.0, 0.0);
	glColor3f (0.0f, 0.0f, 0.0f);
	glPointSize (4.0);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	gluOrtho2D (-TBOX_W, WIN_W - TBOX_W, WIN_H, 0.0);
}

static void glDisplay ()
{
	glClear (GL_COLOR_BUFFER_BIT);
	drawPanel (tbox);
	drawPanel (cbox);
	drawButton (sbox);
	
	// ========================= Testing ==============================
	
	changeColor ( 255, 0, 0 );
	drawLine ( 0, 0, 100, 100 );
	changeColor (255, 255, 0 );
	drawLine ( 100, 100, 200, 200 );
	changeColor ( 0, 0, 255 );
	drawCircle ( 200, 200, 100 );
	changeColor ( 0, 255, 255 );
	drawRect ( 300, 300, 500, 400 );
	changeColor ( 0, 255, 0 );
	drawString ( 200, 50, "HELLO MARLY" );
	
	// ====================== END of Testing ==========================
	
	glFlush ();
}

