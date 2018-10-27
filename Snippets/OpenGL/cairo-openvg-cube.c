/* This example shows how to put a dynamic cairo-generated image
   on an OpenGL texture. It's just a dirty hack on top of an
   article by Rob "phantom" Jones which you can find here:
   http://www.gamedev.net/reference/articles/article2333.asp      */
/* Author: marcoil
   You can find more info on this demo at                         */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glext.h>

#include <vg/openvg.h>
#include <cairo.h>
#include <cairo-openvg.h>

GLuint fbo;					// Our handle to the FBO
GLuint depthBuffer;	// Our handle to the depth render buffer
GLuint img;					// Our handle to a texture
cairo_surface_t	*surface;

const int width = 512;		// The hight of the texture we'll be rendering to
const int height = 512;		// The width of the texture we'll be rendering to

int winwidth = 600;
int winheight = 400;

// Used for drawing the 3D cube with our rendered texture on it
GLfloat	xrot = 0;			// X Rotation
GLfloat	yrot = 0;			// Y Rotation
GLfloat zrot = 0;
GLfloat xspeed = 0.4f;		// X Rotation Speed
GLfloat yspeed = 0.2f;		// Y Rotation Speed
GLfloat zspeed = 0.3f;    // Z Rotation Speed

void 
drawClock(cairo_t * cr)
{
  /* clock drawing code from http://www.cairographics.org/SDLCLock */

  /* store the current time */
  time_t rawtime;
  struct tm *timeinfo;
  double minutes, hours, seconds;

  time(&rawtime);
  
  /* In newer versions of Visual Studio localtime(..) is deprecated. */
  /* Use localtime_s instead. See MSDN. */
  timeinfo = localtime (&rawtime);

  /* compute the angles for the indicators of our clock */
  minutes = timeinfo->tm_min * M_PI / 30;
  hours = timeinfo->tm_hour * M_PI / 6;
  seconds = timeinfo->tm_sec * M_PI / 30;

  /* Clear our surface */
  cairo_set_operator (cr, CAIRO_OPERATOR_CLEAR);
  cairo_paint(cr);

  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);

  /* who doesn't want all those nice line settings :) */
  cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_line_width(cr, 0.1);

  /* translate to the center of the rendering context and draw a black */
  /* clock outline */
  cairo_set_source_rgba(cr, 0, 0, 0, 1);
  cairo_translate(cr, 0.5, 0.5);
  cairo_arc(cr, 0, 0, 0.4, 0, M_PI * 2);
  cairo_stroke(cr);

  /* draw a white dot on the current second. */
  cairo_set_source_rgba(cr, 1, 1, 1, 0.6);
  cairo_arc(cr, sin(seconds) * 0.4, -cos(seconds) * 0.4, 0.05, 0,
	    M_PI * 2);
  cairo_fill(cr);

  /* draw the minutes indicator */
  cairo_set_source_rgba(cr, 0.2, 0.2, 1, 0.6);
  cairo_move_to(cr, 0, 0);
  cairo_line_to(cr, sin(minutes) * 0.4, -cos(minutes) * 0.4);
  cairo_stroke(cr);

  /* draw the hours indicator      */
  cairo_move_to(cr, 0, 0);
  cairo_line_to(cr, sin(hours) * 0.2, -cos(hours) * 0.2);
  cairo_stroke(cr);
}

void init(GLvoid)     
{
	glShadeModel(GL_SMOOTH);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClearDepth(1.0f);					
	glEnable(GL_DEPTH_TEST);			
	glDepthFunc(GL_LEQUAL);				
	glViewport(0,0,winwidth,winheight);
	
	// Setup our FBO
	glGenFramebuffersEXT(1, &fbo);
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);

	// Create the render buffer for depth	and stencil
	glGenRenderbuffersEXT(1, &depthBuffer);
	glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, depthBuffer);
	glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_STENCIL_EXT, width, height);

	// Now setup a texture to render to
	glGenTextures(1, &img);
	glBindTexture(GL_TEXTURE_2D, img);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,  width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

	// And attach it to the FBO so we can render to it
	glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, img, 0);

	// Attach the depth render buffer to the FBO as its depth attachment
	glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depthBuffer);
	// Attach the same render buffer to the FBO as its stencil attachment
	glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depthBuffer);

	GLenum status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
	if(status != GL_FRAMEBUFFER_COMPLETE_EXT) {
	  printf("Framebuffer creation incomplete!\n");
		exit(1);
	}
	
	// Now that we have a working FBO, start ShivaVG on it
	vgCreateContextSH (width, height);
	
	// Create the cairo surface
	surface = cairo_openvg_surface_create (width, height);

	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);	// Unbind the FBO for now
}

void ShutDown(GLvoid)
{
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);
	// Cleaning up the cairo surface and the ShivaVG context
	// must be done while the framebuffer is active
		cairo_surface_destroy (surface);
		vgDestroyContextSH ();
	
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
	glDeleteFramebuffersEXT(1, &fbo);
	glDeleteRenderbuffersEXT(1, &depthBuffer);
	glDeleteTextures(1,&img);
}

void reshape(int w,int h)
{
  winwidth = w;
  winheight = h;
	glViewport( 0, 0, w, h );
	glMatrixMode(GL_PROJECTION);	
	glLoadIdentity();					
	if ( h==0 )							
		gluPerspective(80,(float)w,1.0,5000.0);
	else
		gluPerspective(80,(float)w/(float)h,1.0,5000.0);
	glMatrixMode(GL_MODELVIEW);	
	glLoadIdentity();					
}

void keyboard(unsigned char key,int x,int y)  
{
	switch(key)
	{
	case 27:				// When Escape Is Pressed...
		ShutDown();
		exit(0);			// Exit The Program
		break;				
	default:				
		break;
	}
}

void idle(void)
{
	glutPostRedisplay();
}

void display(void)   
{
	// First we bind the FBO so we can render to it
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fbo);
	
	// Save the view port and set it to the size of the texture
	glPushAttrib(GL_VIEWPORT_BIT);
	glViewport(0,0,width,height);
	
  // We need to set the correct matrices that were set
  // in the ShivaVG initialization	
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();  // To keep correct perspective for 3D scene
	glLoadIdentity();
	gluOrtho2D(0,width,0,height);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	// Then render the cairo scene
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClearDepth(1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );

 	cairo_t *cr = cairo_create (surface);

	cairo_scale(cr, width, height);
	cairo_set_source_rgb(cr, 0.8, 0.8, 0.8);
	cairo_rectangle(cr, 0,0, 1,1);
	cairo_fill(cr);
	drawClock(cr);

  cairo_destroy (cr);

	// Restore old view port and set rendering back to default frame buffer
	glPopAttrib();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

	glClearColor(0.2f, 0.2f, 0.2f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	// Clear Screen And Depth Buffer
	
	// Now bind the texture to use it
	glBindTexture(GL_TEXTURE_2D, img);
	glEnable(GL_TEXTURE_2D);

	glTranslatef(0.0f,0.0f,-2.0f);
	glRotatef(-xrot,1.0f,0.0f,0.0f);
	glRotatef(-yrot,0.0f,1.0f,0.0f);
	glRotatef(-zrot,0.0f,0.0f,1.0f);

	glColor4f(1.0f,1.0f,1.0f,1.0f);

	// This time it's a textured spinning cube!
	// The texture being the scene we just rendered!
	glBegin(GL_QUADS);
		// Front Face
		glNormal3f( 0.0f, 0.0f, 1.0);
		glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5f, -0.5,  0.5);
		glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5, -0.5,  0.5);
		glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5,  0.5,  0.5);
		glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5,  0.5,  0.5);
		// Back Face
		glNormal3f( 0.0f, 0.0f,-1.0);
		glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5, -0.5);
		glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
		glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
		glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5, -0.5);
		// Top Face
		glNormal3f( 0.0f, 1.0, 0.0f);
		glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
		glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5,  0.5,  0.5);
		glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5,  0.5,  0.5);
		glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
		// Bottom Face
		glNormal3f( 0.0f,-1.0, 0.0f);
		glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5, -0.5, -0.5);
		glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5, -0.5, -0.5);
		glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5,  0.5);
		glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5,  0.5);
		// Right face
		glNormal3f( 1.0, 0.0f, 0.0f);
		glTexCoord2f(1.0f, 0.0f); glVertex3f( 0.5, -0.5, -0.5);
		glTexCoord2f(1.0f, 1.0f); glVertex3f( 0.5,  0.5, -0.5);
		glTexCoord2f(0.0f, 1.0f); glVertex3f( 0.5,  0.5,  0.5);
		glTexCoord2f(0.0f, 0.0f); glVertex3f( 0.5, -0.5,  0.5);
		// Left Face
		glNormal3f(-1.0, 0.0f, 0.0f);
		glTexCoord2f(0.0f, 0.0f); glVertex3f(-0.5, -0.5, -0.5);
		glTexCoord2f(1.0f, 0.0f); glVertex3f(-0.5, -0.5,  0.5);
		glTexCoord2f(1.0f, 1.0f); glVertex3f(-0.5,  0.5,  0.5);
		glTexCoord2f(0.0f, 1.0f); glVertex3f(-0.5,  0.5, -0.5);
	glEnd();

	glDisable(GL_TEXTURE_2D);
	
	xrot+=xspeed;
	yrot+=yspeed;
	zrot+=zspeed;

	glutSwapBuffers ( );
	// Swap The Buffers To Not Be Left With A Clear Screen
}

int main(int argc, char* argv[])
{
	
	glutInit(&argc, (char**)argv);
	glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA |
                       GLUT_STENCIL | GLUT_MULTISAMPLE );		// Display Mode
	glutInitWindowSize(winwidth,winheight);
	glutCreateWindow( "FrameBuffer Object Example - Press ESC to exit" );

	init();
	
	// Setup the various call back functions GLUT requires
	glutDisplayFunc     ( display );  
	glutReshapeFunc     ( reshape );
	glutKeyboardFunc    ( keyboard );
	glutIdleFunc		( idle );
	glutMainLoop        ( );			// Run the main GLUT loop for rendering

	return 0;
}

