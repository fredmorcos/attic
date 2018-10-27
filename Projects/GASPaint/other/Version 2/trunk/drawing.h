#ifndef _DRAWING_H_
#define _DRAWING_H_

#include <GL/glut.h>
#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

/* Font Type and Size */
#define FONT_TYPE_AND_SIZE GLUT_BITMAP_9_BY_15
/* IMPORTANT: change this according to above macro */
#define FONT_H 9
#define FONT_W 15

#define DEG_TO_RAD (3.14)/180

void drawText ( int x, int y, char *p );
void drawString ( int x, int y, char *string );
void drawCircle ( int x0, int y0, int x1, int y1 );
void drawRect ( int x0, int y0, int x1, int y1);
void drawLine ( int x0, int y0, int x1, int y1);

/* Old Filling function signature, don't delete */
//void fill ( GdkColor initial, GdkColor final, int x, int y );

void fill ( int x, int y );

void drawCommand ( char *command );

#endif
