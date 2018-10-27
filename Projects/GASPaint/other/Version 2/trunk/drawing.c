#include "drawing.h"
#include "config.h"

#include <gtk/gtk.h>
#include <GL/gl.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "drawarea.h"
#include "public.h"

/* implements various of the drawing functions */

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
 * void drawCircle ( int x, int y, int radius ) - Draws a circle with the specified radius with the center at the specified point.
 * @ int x: x position.
 * @ int y: y position.
 * @ int radius: The radius of the circle.
 *
 * Draws a circle with the specified radius with the center at the specified point.
 **/
void drawCircle ( int x0, int y0, int x1, int y1 )
{
	int radius = sqrt ( ((x1 - x0)*(x1 - x0)) + ((y1 - y0)*(y1 - y0)));
	float incr = 50.0/radius;
	glBegin (GL_LINE_STRIP);
	float i = 0;
	float degInRad;
	while (i <= 1100)
	{
		degInRad = (int)(i) * DEG_TO_RAD;
		glVertex2f (cos (degInRad) * radius + x0, sin (degInRad) * radius + y0);
		i = i + incr;
	}
	glEnd();
}

void changeColor ( GdkColor color )
{
	glColor3f ( color.red/255, color.green/255, color.blue/255 );
}


/**
 * void fill ( GdkColor initial, GdkColor final, int x, int y )
 * int RGB0: the RGB of the original point
 * int RGB1: the new color for all the points around
 * @ int x: x position.
 * @ int y: y position.
 *
 * fills the area around the x,y that had the initial color with the final color.
 **/
void fillh ( int R0, int G0, int B0, int R1, int G1, int B1, int x, int y )
{	
	GdkColor initial, final;
	initial.red = R0;
	initial.green = G0;
	initial.blue = B0;
	final.red = R1;
	final.green = G1;
	final.blue = B1;
	
	float *pixel_color = malloc (sizeof (float) * 3 );
	// if (pixel_color == NULL) return;
	glReadPixels (x, y, 1, 1, GL_RGB, GL_FLOAT, pixel_color);
	
	if (((int)(pixel_color[0] * 255) == initial.red && (int)(pixel_color[1] * 255) == initial.green && (int)(pixel_color[2] * 255) == initial.blue)
		&&
		!((int)(pixel_color[0] * 255) == final.red && (int)(pixel_color[1] * 255) == final.green && (int)(pixel_color[2] * 255) == final.blue))
	{
		glBegin ( GL_POINT );
			glVertex2i (x, 640 - y);
		glEnd ();
		
		fillh (R0, G0, B0, R1, G1, B1, x + 1, y);
		fillh (R0, G0, B0, R1, G1, B1, x, y + 1);
		fillh (R0, G0, B0, R1, G1, B1, x, y - 1);
		fillh (R0, G0, B0, R1, G1, B1, x - 1, y);
	}
	free ( pixel_color );
}

/* gets the current color of the pixel clicked and calls fillh to implement filling */
void fill ( int x, int y )
{
	y=640-y;
	float *pixel_color = malloc ( sizeof( float )*3 );
	
	glReadPixels( x, y, 1, 1, GL_RGB, GL_FLOAT, pixel_color );
	
	//printf ( "Should be int: %d\n", (int)(pixel_color[0]*255 ));
	if ( ! ( pixel_color[0]*255 == current_color.red/255 && pixel_color[1]*255 == current_color.green/255 && pixel_color[2]*255 == current_color.blue/256 ) )
	{
		fillh ( (int)(pixel_color[0]*255), (int)(pixel_color[1]*255), (int)(pixel_color[2]*255), current_color.red/256, current_color.green/256, current_color.blue/256 , x, y );
	}
	free ( pixel_color );
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
		
	//glRecti(x0,y0,x1,y1);
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
 * void drawCommand ( char *command ) - Takes a command and send it to the appropriate method to execute it.
 * @ char *command: string containing the command.
 *
 * Takes a command, written in the format we agreed on, and checks it applies to which condition and then send it to 
 * the appropriate method to execute it and draw what is requested.
 **/
void drawCommand ( char *command)
{	
	char *tmp = malloc (sizeof (char) * strlen (command) + 2);
	strcpy (tmp, command);
	strcat (tmp, "\0");
	
	// printf ("Command @ draw Command: %s\n", tmp);
	
	char *temp = strtok (tmp, " ");
	
	if ( strcmp ( temp, "line" ) == 0)
	{ 
		char *temp1 = strtok (NULL, " ");
		char *temp2 = strtok (NULL, " ");
		char *temp3 = strtok (NULL, " ");
		char *temp4 = strtok (NULL, " ");	
		drawLine (atoi(temp1), atoi(temp2), atoi(temp3), atoi(temp4));
	}
	else if ( strcmp ( temp, "point" ) == 0)
	{
		char *temp1 = strtok (NULL, " ");
		char *temp2 = strtok (NULL, " ");
		//drawPoint (atoi(temp1), atoi(temp2));
	}
	else if ( strcmp ( temp, "rect" ) == 0)
	{
		char *temp1 = strtok (NULL, " ");
		char *temp2 = strtok (NULL, " ");
		char *temp3 = strtok (NULL, " ");
		char *temp4 = strtok (NULL, " ");
		drawRect (atoi(temp1), atoi(temp2), atoi(temp3), atoi(temp4));
	}
	else if ( strcmp ( temp, "circ" ) == 0)
	{
		char *temp1 = strtok (NULL, " ");
		char *temp2 = strtok (NULL, " ");
		char *temp3 = strtok (NULL, " ");
		char *temp4 = strtok (NULL, " ");
		drawCircle (atoi(temp1), atoi(temp2), atoi(temp3), atoi(temp4));
	}
	else if ( strcmp ( temp, "fill" ) == 0)
	{
		char *temp1 = strtok (NULL, " ");
		char *temp2 = strtok (NULL, " ");
		fill (atoi(temp1), atoi(temp2));
	}
	else if ( strcmp ( temp, "color" ) == 0)
	{
		/* the color will be saved in format #rrggbb HEX */
		char *temp1 = strtok (NULL, " ");
		// printf ("temp1: %s\n", temp1);
		gdk_color_parse (temp1, &current_color);
		draw_area_set_color ();
	}
	free (temp);
}
