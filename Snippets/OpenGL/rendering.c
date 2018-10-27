/*******************************************************************************
**3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
**      10        20        30        40        50        60        70        80
**
** file:
**    opengl-rendering.c
**
** author:
**    Mirco "MacSlow" Mueller <macslow@bangang.de>
**
** copyright (C) Mirco Mueller, July 2006, placed under the terms of the LGPL
**
*******************************************************************************/

#include <stdio.h>

#include "SDL.h"
#include "SDL_opengl.h"
#include "opengl-rendering.h"

/* now this here is _really_ nasty, but I'm too lazy to do it right */
#define GL_TEXTURE_RECTANGLE_ARB 0x84F5

void
init_gl ()
{
	printf ("OpenGL version: %s\n", glGetString (GL_VERSION));
	printf ("OpenGL vendor: %s\n", glGetString (GL_VENDOR));
	printf ("OpenGL renderer: %s\n", glGetString (GL_RENDERER));

	glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
	glDisable (GL_DEPTH_TEST);
    glEnable (GL_BLEND);
	glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable (GL_TEXTURE_RECTANGLE_ARB);
}

void
draw_func (int iWidth,
		   int iHeight,
		   unsigned char* pucSurfaceData,
		   unsigned int uiTextureId)
{
	if (!pucSurfaceData)
	{
		printf ("draw_func() - No valid pointer to surface-data passed\n");
		return;
	}

	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
	glClear (GL_COLOR_BUFFER_BIT);

	glPushMatrix ();

	glBindTexture (GL_TEXTURE_RECTANGLE_ARB, uiTextureId);
	glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
				  0,
				  GL_RGBA,
				  iWidth,
				  iHeight,
				  0,
				  GL_BGRA,
				  GL_UNSIGNED_BYTE,
				  pucSurfaceData);

	glColor3f (0.25f, 0.5f, 1.0f);
	glBegin (GL_QUADS);
	glTexCoord2f (0.0f, 0.0f);
	glVertex2f (0.0f, 0.0f);
	glTexCoord2f ((GLfloat) iWidth, 0.0f);
	glVertex2f (1.0f, 0.0f);
	glTexCoord2f ((GLfloat) iWidth, (GLfloat) iHeight);
	glVertex2f (1.0f, 1.0f);
	glTexCoord2f (0.0f, (GLfloat) iHeight);
	glVertex2f (0.0f, 1.0f);
	glEnd ();

	glPopMatrix ();

    SDL_GL_SwapBuffers();
}

void
resize_func (int iWidth,
			 int iHeight,
			 unsigned int* puiTextureId)
{
	glViewport (0, 0, iWidth, iHeight);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glOrtho (0.0f, 1.0f, 0.0f, 1.0f, -1.0f, 1.0f);

	glClear (GL_COLOR_BUFFER_BIT);

	glDeleteTextures (1, puiTextureId);
	glGenTextures (1, puiTextureId);
	glBindTexture (GL_TEXTURE_RECTANGLE_ARB, *puiTextureId);
	glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
				  0,
				  GL_RGBA,
				  iWidth,
				  iHeight,
				  0,
				  GL_BGRA,
				  GL_UNSIGNED_BYTE,
				  NULL);
	glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
}
