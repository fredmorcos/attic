#include "drawing.h"



/**
 * void drawText () - Draws the specified text at the specified position.
 * @ int x: x position.
 * @ int y: y position.
 * @ char *p: The string to be drawn.
 *
 * Draws the specified text ate the specified position. 
 * NOTE: Deprecated, see drawString.
 **/
void drawText ( int x, int y, char *p )
{
	glRasterPos2i ( x, y );
	int i=0;
	while ( p[i]!='\0' )
	{
		glutBitmapCharacter( FONT_TYPE_AND_SIZE, p[i] );
		i++;
	}
}


/**
 * void drawString ( int x, int y, char *string ) - Draws the specified text at the specified position.
 * @ int x: x position.
 * @ int y: y position.
 * @ char *string: The string to be drawn.
 *
 * Draws the specified text ate the specified position.
 **/
void drawString ( int x, int y, char *string )
{
	/* TODO:
	 * setting the color temporarily, as all text should be in black,
	 * at least for now. we can set some #define macro for it?
	 * i think u should put the color back as it was after drawing.
	 * --> when i remove the color line it gives a runtime error.
	 */
	glColor3f (0.0, 0.0, 0.0);

	glRasterPos2i ( x, y );
	glutBitmapString ( FONT_TYPE_AND_SIZE, string );
}

/**
 * void DrawPanel () - Draws the specified panel on the window.
 * @panel p: the panel to be drawn
 * 
 * Will get the color and position of the panel and will draw it on the window
 **/
void drawPanel (panel p)
{
	glColor3f (p.bg.red, p.bg.green, p.bg.blue);
	glRecti (p.x1, p.y1, p.x2, p.y2);	
}

/**
 * void drawPanelText () - Draws the text in the middle of the panel.
 * @panel p: the panel where the text will be centered and drawn on
 * @char *t: the text to be drawn on the panel
 * 
 * This will draw the text *t on panel p.
 * NOTE: this is not very accurate!
 **/
void drawPanelText (panel p, char *t)
{
	/* too much stupidity here, doesn't work correctly */
	/* int t_len = 0, t_top = 0, t_left = 0; */
	
	/* we get the width of the graphical text length */
	/* t_len = strlen (t);
	t_len *= FONT_W;
	
	t_top = (p.y2 - p.y1 - FONT_H) + p.y1;
	t_left = ((p.x2 - p.x1 - t_len) / 2) + p.x1;
	
	#ifdef __DEBUG
	printf ("Text x: %d, y: %d\n", t_left, t_top);
	#endif
	
	drawString (t_left, t_top, t);
	*/
	
	drawString (p.x1 + 4, p.y1 + 4 + FONT_H, t);
}

/**
 * void drawButton () - Will draw a button with it's text
 * @button b: the button to be drawn
 * 
 * Draws the given button
 **/
void drawButton (button b)
{
	drawPanel (b.container);
	drawPanelText (b.container, b.text);
}

/**
 * void drawCircle ( int x, int y, int radius ) - Draws a circle with the specified radius with the center at the specified point.
 * @ int x: x position.
 * @ int y: y position.
 * @ int radius: The radius of the circle.
 *
 * Draws a circle with the specified radius with the center at the specified point.
 **/
void drawCircle ( int x, int y, int radius )
{
	glBegin (GL_LINE_LOOP);
	int i = 0;
	while (i < 360)
	{
		float degInRad = i * DEG_TO_RAD;
		glVertex2f (cos (degInRad) * radius + x, sin (degInRad) * radius + y);
		i+=1;
	}
	glEnd();
}


/**
 * void drawRect ( int x0, int y0, int x1, int y1) - Draws a rectangle with the corners at the specified points.
 * @ int x0: x position of start point.
 * @ int y0: y position of start point.
 * @ int x1: x position of end point.
 * @ int y1: y position of end point.
 *
 * Draws a rectangle with the corners at the specified points.
 **/
void drawRect ( int x0, int y0, int x1, int y1)
{
	glBegin ( GL_LINE_STRIP );
		glVertex2i ( x0, y0 );
		glVertex2i ( x1, y0 );
		glVertex2i ( x1, y1 );
		glVertex2i ( x0, y1 );
		glVertex2i ( x0, y0 );
	glEnd ();
}


/**
 * void drawLine ( int x0, int y0, int x1, int y1) - Draws a line between the 2 points.
 * @ int x0: x position of start point.
 * @ int y0: y position of start point.
 * @ int x1: x position of end point.
 * @ int y1: y position of end point.
 *
 * Draws a line between the 2 points.
 **/
void drawLine ( int x0, int y0, int x1, int y1)
{
	glBegin ( GL_LINE );
		glVertex2i ( x0, y0 );
		glVertex2i ( x1, y1 );
	glEnd ();
}


/**
 * void changeColor ( int R, int G, int B) - Changes the drawing color to R G B.
 * @ int R: Red component value.
 * @ int G: Green component value.
 * @ int B: Blue component value.
 *
 * Changes the drawing color to R G B.
 **/
void changeColor ( int R, int G, int B)
{
	glColor3f ( R/255, G/255, B/255 );
}

