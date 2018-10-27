#include "mouse.h"

void processMouse (int button, int state, int x, int y) // trying to make the the strange string that appears go away... still can't..
{	
	if ((state == GLUT_DOWN)/* && (button == GLUT_RIGHT_BUTTON)*/) 
	{
		if (button == GLUT_RIGHT_BUTTON)
			printf ("heyyyyy");
		else printf ("working aho  "); // just to prove the function is working
		#define BUF_SIZE 20
		char line [BUF_SIZE];

		sprintf (line, "Hello :)");

		#ifdef __DEBUG
		printf ("%s\n", line);
		#endif

		drawButton (sbox);
	}
}



/**
 * func processMousePassiveMotion - prints on the console were the mouse is pointing at
 * @x: the x-axis
 * @y: the y-axis
 *
 * function prints the position of the mouse while moving on the screen
 **/
void processMousePassiveMotion (int x, int y) 
{
	#define BUF_SIZE 20
	char line [BUF_SIZE];
	
	x = getMouseX ( x );
	y = getMouseY ( y );
	
	sprintf (line, "X: %d\nY: %d", x, y);
	#ifdef __DEBUG
	//printf ("%s\n", line);	// we dun need this here anymore i guess (Andrew)
	#endif

	if (x >= 0)
		sbox.text = line;
	else
		sprintf (line, "Hello :)");

	drawButton (sbox);
}


/**
 * func int getMouseX ( int x ) - converts the mouse click X coordinate relative to the Ortho
 * @x: the x-axis
 *
 * converts the mouse click X coordinate relative to the Ortho
 **/
int getMouseX ( int x )
{
	return x-TBOX_W;
}


/**
 * func int getMouseY ( int y ) - converts the mouse click Y coordinate relative to the Ortho
 * @x: the y-axis
 *
 * converts the mouse click Y coordinate relative to the Ortho
 **/
int getMouseY ( int y )
{
	return y;
}
