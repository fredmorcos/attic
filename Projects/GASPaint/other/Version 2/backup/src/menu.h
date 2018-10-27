#ifndef _MENU_H_
#define _MENU_H_

#include <GL/glut.h>

#include "main.h"
#include "config.h"
#include "ui.h"
#include "stdio.h"
#include "drawing.h"

void fileMenu (void);					/* Creates file menu */
void processMenuEvents (int option);	/* process the event asked by the menu selected */
void processMenuStatus (int status, int x, int y);

#endif
