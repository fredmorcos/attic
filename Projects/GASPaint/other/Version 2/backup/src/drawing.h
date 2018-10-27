#ifndef _DRAWING_H_
#define _DRAWING_H_

#include <GL/glut.h>

#include <math.h>
#include <string.h>			/* for string operations: strlen (), etc... */

#include "panel.h"
#include "button.h"
#include "config.h"

/* Font Type and Size */
#define FONT_TYPE_AND_SIZE GLUT_BITMAP_9_BY_15
/* IMPORTANT: change this according to above macro */
#define FONT_H 9
#define FONT_W 15

#define DEG_TO_RAD (3.14)/180

void drawText ( int x, int y, char *p );
void drawString ( int x, int y, char *string );
void drawCircle ( int x, int y, int radius );
void drawRect ( int x0, int y0, int x1, int y1);
void drawLine ( int x0, int y0, int x1, int y1);
void changeColor ( int R, int G, int B);

void drawPanel (panel);
void drawPanelText (panel, char*);
void drawButton (button);

#endif
