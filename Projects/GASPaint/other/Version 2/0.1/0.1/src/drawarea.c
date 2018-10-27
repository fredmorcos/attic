/*
 * This file is part of GasPaint
 * 
 * GasPaint OpenGL/Gtk+ based paint-like program.
 * Copyright (C) 2008  	Frederic-Gerald Morcos
 						Andrew Botros Boktor
 						Marleine Mounir Daoud
 * 
 * GasPaint is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * GasPaint is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GasPaint.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>
#include <GL/gl.h>
#include <GL/glut.h>
#include <string.h>

#include "drawarea.h"
#include "public.h"
#include "config.h"
#include "drawing.h"
#include "event.h"

/* our drawing area */

/* configures the openGL options for our drawing area */
void draw_area_configure ()
{
	GdkGLContext *glcontext = gtk_widget_get_gl_context (draw_area);
	GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (draw_area);
	
	if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext))
		g_assert_not_reached ();
		
	glClearColor (1.0, 1.0, 1.0, 0.0);
	glColor3f (0.0f, 0.0f, 0.0f);
	glPointSize (1.0);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D (0.0, draw_area->allocation.width, draw_area->allocation.height, 0.0);
	
	gdk_gl_drawable_gl_end (gldrawable);
}

/* is called when the drawing area is clicked,
 * manages the various clicks for the various tools
 */
void draw_area_clicked (GtkWidget *widget, GdkEventButton *event)
{
	GdkGLContext *glcontext = gtk_widget_get_gl_context (draw_area);
	GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (draw_area);

	if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext))
		g_assert_not_reached ();
		
	gint x = event->x, y = event->y;
	
	if (num_clicks == 0)
	{
		num_clicks++;
		current_x0 = x;
		current_y0 = y;
		
		if (current_tool == FILL)
		{
			char command [25];
			num_clicks = 0;
			sprintf (command, "fill %d %d", current_x0, current_y0);
			add_event (command);
			draw_area_expose ();
		}
	}
	else if (num_clicks == 1)
	{
		num_clicks = 0;
		current_x1 = x;
		current_y1 = y;
		
		char command [25];		/* 3*4 digits + 4 (spaces) + 4 chars (command name) + \0 */
		
		if (current_tool == LINE)
			sprintf (command, "line %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
		else if (current_tool == RECT)
			sprintf (command, "rect %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
		else if (current_tool == CIRC)
			sprintf (command, "circ %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
		else if (current_tool == POLY)
		{
			num_clicks = 1;
			sprintf (command, "line %d %d %d %d", current_x0, current_y0, current_x1, current_y1);
			current_x0 = current_x1;
			current_y0 = current_y1;
		}
		add_event (command);
		draw_area_expose ();
	}
	else
	{
		current_x0 = -1;
		current_y0 = -1;
		current_x1 = -1;
		current_y1 = -1;
		num_clicks = 0;
	}
	
	gdk_gl_drawable_gl_end (gldrawable);
}

/* is called when the drawing area is exposed,
 * like when it needs to be redrawn
 */
void draw_area_expose ()
{
	GdkGLContext *glcontext = gtk_widget_get_gl_context (draw_area);
	GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (draw_area);

	if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext))
		g_assert_not_reached ();
	
	glClear (GL_COLOR_BUFFER_BIT);
	drawEvents ();

	//============= Testing code added by Andrew ======================
	
	/* if any1 sees this, PLEASE i need help, please check the code in gaspaint-trunk/examples/opengl_example.c
	 * it's a copy/paste from the code here, it works there but not here, i need help finding the error.
	 */
	
	/*
	drawRect ( 15, 15, 100, 100 );
	GdkColor c1, c2;
	c1.red = 255;
	c1.green = 255;
	c1.blue = 255;
	
	current_color.red = 0;
	current_color.green = 0;
	current_color.blue = 255;
	changeColor ( current_color );
		
		glBegin ( GL_LINE );
       glVertex2i ( 0 ,0 );
       glVertex2i ( 110 ,110 );
       glEnd ();
       
       
       
       glFlush();                          // send all output to display
       printf ( "HELLO\n" );
       float *R = malloc ( 99 );
       glReadPixels( 0, 0, 1, 1, GL_RGB, GL_FLOAT, R );
       printf ( "R[0]: %f\n", R[0] );
       printf ( "R[1]: %f\n", R[1] );
       printf ( "R[2]: %f\n", R[2] );
       free ( R );
	//fill ( c1, current_color, 30, 100 );
	
	//============= END >> Testing code added by Andrew ===============
	*/
	
	if (gdk_gl_drawable_is_double_buffered (gldrawable))
		gdk_gl_drawable_swap_buffers (gldrawable);
	else
		glFlush ();

	gdk_gl_drawable_gl_end (gldrawable);
}

/* called when the mouse is moving over the drawing area */
void draw_area_motion (GtkWidget *widget, GdkEventMotion *event, GtkWidget *label)
{
	gint x = event->x, y = event->y;
	char pos [20];
	
	sprintf (pos, "X: %d, Y: %d", x, y);
	gtk_label_set_label (GTK_LABEL (label), pos); 
}

void draw_area_set_color ()
{
	GdkGLContext *glcontext = gtk_widget_get_gl_context (draw_area);
	GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (draw_area);

	if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext))
		g_assert_not_reached ();

	guint r = current_color.red;
	guint g = current_color.green;
	guint b = current_color.blue;
	
	/* printf ("%d, %d, %d\n", r, g, b);
	printf ("%f, %f, %f\n", r/255.0, g/255.0, b/255.0);
	*/
	
	glColor3f ((r/256.0)/256.0, (g/256.0)/256.0, (b/256.0)/256.0);
	
	gdk_gl_drawable_gl_end (gldrawable);
}
