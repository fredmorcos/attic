/*
 * This file is part of pacman-ng
 * 
 * Hex packet sniffer
 * Copyright (C) 2007  Frederic-Gerald Morcos
 * 
 * pacman-ng is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * pacman-ng is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with pacman-ng.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "texture.h"

#include <stdio.h>
#include <stdlib.h>
#include <GL/glut.h>
#include "global.h"


unsigned int textureFile (const char *pcFile, int iWidth, int iHeight)
{
	unsigned int uiTexture;
	
	GLubyte *data;
	FILE *file;

	file = fopen (pcFile, "rb");
	if (file == NULL) return 0;

	data = malloc (3 * iWidth * iHeight);

	fread (data, iWidth * iHeight * 3, 1, file);
	fclose (file);

	glGenTextures (1, &uiTexture);
	glBindTexture (GL_TEXTURE_2D, uiTexture);
	glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D (GL_TEXTURE_2D, 0, GL_RGB, iWidth, iHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
	glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	//gluBuild2DMipmaps (GL_TEXTURE_2D, 3, iWidth, iHeight, GL_RGB, GL_UNSIGNED_BYTE, data);
	free (data);

	return uiTexture;
}

GLuint LoadTextureRAW( const char * filename, int wrap )
{
  GLuint texture;
  int width, height;
  unsigned char * data;
  FILE * file;

  // open texture data
  file = fopen( filename, "rb" );
  if ( file == NULL ) return 0;

  // allocate buffer
  width = 256;
  height = 256;
  data = malloc( width * height * 3 );

  // read texture data
  fread( data, width * height * 3, 1, file );
  fclose( file );

  // allocate a texture name
  glGenTextures( 1, &texture );

  // select our current texture
  glBindTexture( GL_TEXTURE_2D, texture );

  // select modulate to mix texture with color for shading
  glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

  // when texture area is small, bilinear filter the closest MIP map
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                   GL_LINEAR_MIPMAP_NEAREST );
  // when texture area is large, bilinear filter the first MIP map
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

  // if wrap is true, the texture wraps over at the edges (repeat)
  //       ... false, the texture ends at the edges (clamp)
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,
                   wrap ? GL_REPEAT : GL_CLAMP );
  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,
                   wrap ? GL_REPEAT : GL_CLAMP );

  // build our texture MIP maps
  gluBuild2DMipmaps( GL_TEXTURE_2D, 3, width,
    height, GL_RGB, GL_UNSIGNED_BYTE, data );

  // free buffer
  free( data );

  return texture;

}

