#include "menu.h"

/**
 * func processMenuEvents - process the event asked by the menu selected
 * @option: the integer that the menu selected returns
 *
 * function includes cases for each menu with a specific integer that 
 * the menu selected returns to this function
 **/

void processMenuEvents (int option) 
{
	switch (option) 
	{
		case 1: 
			glClear (GL_COLOR_BUFFER_BIT); break;		/* to open a new page */
		case 2: 
			break;										/* to load a saved page */
		case 3: 
			break;										/* to save a page */
		case 4: 
			glutLeaveMainLoop(); break;					/* to exit */
		case 5:
			break;										/* to undo */
		case 6: 
			break;										/* to redo */
	}
}

void fileMenu () 
{
	int menu, submenufile, submenuedit;

	submenufile = glutCreateMenu (processMenuEvents);
	glutAddMenuEntry ("New",1);
	glutAddMenuEntry ("Load",2);
	glutAddMenuEntry ("Save",3);
	glutAddMenuEntry ("Exit",4);
	
	submenuedit = glutCreateMenu (processMenuEvents);
	glutAddMenuEntry ("Undo",5);
	glutAddMenuEntry ("Redo",6);
	
	menu = glutCreateMenu (processMenuEvents);
	glutAddSubMenu ("File",submenufile);
	glutAddSubMenu ("Edit",submenuedit);

	glutAttachMenu (GLUT_RIGHT_BUTTON);
}

void processMenuStatus (int status, int x, int y) // trying to make the strange string that appears go away... still can't..
{

	if (status == GLUT_MENU_IN_USE)
	{
		#define BUF_SIZE 20
		char line [BUF_SIZE];

		sprintf (line, "X: %d\nY: %d", x, y);

		#ifdef __DEBUG
		printf ("%s\n", line);
		#endif

		drawButton (sbox);
	}

}
