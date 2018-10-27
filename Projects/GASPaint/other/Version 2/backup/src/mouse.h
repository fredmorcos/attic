#ifndef _MOUSE_H_
#define _MOUSE_H_

#include <GL/glut.h>

#include "main.h"
#include "config.h"
#include "ui.h"
#include "stdio.h"
#include "drawing.h"

/* function that prints mouse position on console */
void processMouse (int button, int state, int x, int y);
void processMousePassiveMotion (int x, int y);

int getMouseX ( int x );
int getMouseY ( int y );

#endif
