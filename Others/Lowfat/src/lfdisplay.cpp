////////////////////////////////////////////////////////////////////////////////
//3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
//
//  lowfat - an "engine" for natural document viewing for free desktop-systems
//
//  copyright (c) 2007 Mirco Müller
//
//  lowfat is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  lowfat is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
////////////////////////////////////////////////////////////////////////////////

#include <GL/gl.h>
#include <GL/glu.h>
#include <stdexcept>
#include <exception>

#ifndef GL_CLAMP_TO_EDGE
#define GL_CLAMP_TO_EDGE 0x812F
#endif

#include "SDL.h"
#include "include/lfdisplay.h"

static inline void
setColor (const Color& c)
{
	glColor4f (c.r, c.g, c.b, c.a);
}

static inline void
setVertex (const Vec2f& v)
{
	glVertex2f (v.x, v.y);
}

static inline void
setTexVertex (const Vec2f& p,
	      float u,
	      float v)
{
	glTexCoord2f (u, v);
	glVertex2f (p.x, p.y);
}

static inline void
setTexVertex (float x,
	      float y,
	      float u,
	      float v)
{
	glTexCoord2f (u, v);
	glVertex2f (x, y);
}

static inline void
setVertex (float x,
	   float y)
{
	glVertex2f (x, y);
}

static inline void
setVertex (const Vec2i& v)
{
	glVertex2i (v.x, v.y);
}

static inline void
setTexVertex (const Vec2i& p,
	      float u,
	      float v)
{
	glTexCoord2f (u, v);
	glVertex2i (p.x, p.y);
}

LfDisplayPtr
LfDisplay::make (std::string title,
		 float width,
		 float height,
		 bool fullscreenFlag,
		 int argc,
		 char** argv)
{
	return LfDisplayPtr (new LfDisplay (title,
					    width,
					    height,
					    fullscreenFlag,
					    argc,
					    argv));
}

LfDisplay::~LfDisplay ()
{
	if (surfaceLfDisplayPtr_)
		SDL_FreeSurface (surfaceLfDisplayPtr_);

	SDL_Quit ();
}

int
LfDisplay::getWidth () const
{
	return (width_);
}

int
LfDisplay::getHeight () const
{
	return (height_);
}

bool
LfDisplay::isFullscreen () const
{
	return fullscreenFlag_;
}

std::string
LfDisplay::getTitle () const
{
	return title_;
}

float
LfDisplay::getScaleFactor () const
{
	return scaleFactor_;
}

void
LfDisplay::setScaleFactor (float scaleFactor)
{
	scaleFactor_ = scaleFactor;
}

Vec2f
LfDisplay::getTranslation() const
{
	return translationVector_;
}

void
LfDisplay::setTranslation (const Vec2f& translationVector)
{
	translationVector_ = translationVector;
}

void
LfDisplay::setTranslation (float vecPartX,
			   float vecPartY)
{
	translationVector_.x = vecPartX;
	translationVector_.y = vecPartY;
}

float
LfDisplay::getRotation () const
{
	return rotationAngle_;
}

void
LfDisplay::setRotation (float rotationAngle)
{
	rotationAngle_ = rotationAngle;
}

void
LfDisplay::setTexture (TexturePtr texture,
		       float size)
{
	if (!texture.isNull ())
	{
		glEnable (texture->getTextureTarget ());
		const bool hasTexAlpha = texture->hasAlpha ();
		glTexEnvi (GL_TEXTURE_ENV,
			   GL_TEXTURE_ENV_MODE,
			   hasTexAlpha ? GL_MODULATE : GL_DECAL);
		glTexEnvi (GL_TEXTURE_ENV,
			   GL_TEXTURE_ENV_COLOR,
			   hasTexAlpha ? GL_MODULATE : GL_DECAL);

		int level = 0;
		while (size < 0.5)
		{
			level++;
			size *= 2;
		}
		texture->setMipMapLevel (level);

		glBindTexture (texture->getTextureTarget (),
			       texture->getTextureId ());
		glScalef (scaleFactor_, scaleFactor_, scaleFactor_);
	}
	else
		if (!texturePtr_.isNull ())
			glDisable (texturePtr_->getTextureTarget ());

	texturePtr_ = texture;
}

Color
LfDisplay::getDrawColor () const
{
	return drawColor_;
}

void
LfDisplay::setDrawColor (const Color& c)
{
	setDrawColor (c.r, c.g, c.b, c.a);
}

void
LfDisplay::setDrawColor (float r,
			 float g,
			 float b,
			 float a)
{
	drawColor_.set (r, g, b, a);
	glColor4f (r, g, b, a);
}

void
LfDisplay::drawQuad (const Vec2f& vec1,
		     const Vec2f& vec2,
		     const Vec2f& vec3,
		     const Vec2f& vec4) const
{
	drawQuad (vec1, Color::white,
		  vec2, Color::white,
		  vec3, Color::white,
		  vec4, Color::white);
}

void
LfDisplay::drawQuad (const Vec2f& vec1,
		     const Color& col1,
		     const Vec2f& vec2,
		     const Color& col2,
		     const Vec2f& vec3,
		     const Color& col3,
		     const Vec2f& vec4,
		     const Color& col4) const
{
	if  (texturePtr_.isNull ())
	{
		glBegin (GL_QUADS);
		setColor (col1);
		glVertex2f (vec1.x, vec1.y);
		setColor (col2);
		glVertex2f (vec2.x, vec2.y);
		setColor (col3);
		glVertex2f (vec3.x, vec3.y);
		setColor (col4);
		glVertex2f (vec4.x, vec4.y);
		glEnd ();
	}
	else
	{
		const float twidth (texturePtr_->getWidth ());
		const float theight (texturePtr_->getHeight ());

		Vec2f texA (0.5f / twidth, 0.5f / theight);
		Vec2f texB ((2.0f * twidth - 1.0f) / (twidth * 2.0f),
			    0.5f / theight);
		Vec2f texC ((2.0f * twidth - 1.0f) / (twidth * 2.0f),
			    (2.0f * theight - 1.0f) / (2.0f * theight));
		Vec2f texD (0.5f / twidth,
			    (2.0f * theight - 1.0f) / (2.0f * theight));

		setColor (col1);
		glBegin (GL_QUADS);
		glTexCoord2f (texA.x, texA.y);
		glVertex2f (vec1.x, vec1.y);
		glTexCoord2f (texB.x, texB.y);
		glVertex2f (vec2.x, vec2.y);
		glTexCoord2f (texC.x, texC.y);
		glVertex2f (vec3.x, vec3.y);
		glTexCoord2f (texD.x, texD.y);
		glVertex2f (vec4.x, vec4.y);
		glEnd ();
	}
}

void
LfDisplay::drawRectWrapped (float x,
			    float y,
			    float width,
			    float height)
{
	setColor (drawColor_);
	const Vec2f mid (x + width / 2, y + height / 2);
	Vec2f a (-width / 2, -height / 2);
	Vec2f b (width / 2, -height / 2);

	if (texturePtr_.isNull ())
	{
		glBegin (GL_QUADS);
		setVertex(mid + a);
		setVertex(mid + b);
		setVertex(mid - a);
		setVertex(mid - b);
		glEnd ();
	}
	else
	{
		const float maxS = texturePtr_->getMaxS ();
		const float maxT = texturePtr_->getMaxT ();

		Vec2f texA (0, 0);
		Vec2f texB (maxS, 0);
		Vec2f texC (maxS, maxT);
		Vec2f texD (0, maxT);

		Vec2f vecA(mid + a);
		Vec2f vecB(mid + b);
		Vec2f vecC(mid - a);
		Vec2f vecD(mid - b);

		glTexParameteri (texturePtr_->getTextureTarget (),
				 GL_TEXTURE_WRAP_S,
				 GL_REPEAT);
		glTexParameteri (texturePtr_->getTextureTarget(),
				 GL_TEXTURE_WRAP_T,
				 GL_REPEAT);

		glBegin (GL_QUADS);
		setTexVertex (vecA, texA.x, texA.y);
		setTexVertex (vecB, texB.x, texB.y);
		setTexVertex (vecC, texC.x, texC.y);
		setTexVertex (vecD, texD.x, texD.y);
		glEnd ();

		glTexParameteri (texturePtr_->getTextureTarget (),
				 GL_TEXTURE_WRAP_S,
				 GL_CLAMP_TO_EDGE);
		glTexParameteri (texturePtr_->getTextureTarget (),
				 GL_TEXTURE_WRAP_T,
				 GL_CLAMP_TO_EDGE);
	}
}

void
LfDisplay::drawRectangleMapped (float x,
				float y,
				float width,
				float height,
				float angle)
{
	setColor (drawColor_);

	const Vec2f mid (x + width / 2, y + height / 2);
	Vec2f a(-width / 2, -height / 2);
	Vec2f b(width / 2, -height / 2);
	a.rotate (angle);
	b.rotate (angle);

	if (texturePtr_.isNull ())
	{
		glBegin (GL_QUADS);
		setVertex (mid + a);
		setVertex (mid + b);
		setVertex (mid - a);
		setVertex (mid - b);
		glEnd ();
	}
	else
	{
		float texSize = width / texturePtr_->getWidth ();
		setTexture (texturePtr_, texSize);

		const float maxS = texturePtr_->getMaxS ();
		const float maxT = texturePtr_->getMaxT();
		const float tWidth = texturePtr_->getWidth();
		const float tHeight = texturePtr_->getHeight();
		Vec2f texA( 0.0 / tWidth, 0.0 / tHeight );
		Vec2f texB( maxS - 0.0 / tWidth, 0.0 / tHeight );
		Vec2f texC( maxS - 0.0 / tWidth, maxT - 0.0 / tHeight );
		Vec2f texD( 0.0 / tWidth, maxT - 0.0 / tHeight);

		Vec2f vecA (mid + a);
		Vec2f vecB (mid + b);
		Vec2f vecC (mid - a);
		Vec2f vecD (mid - b);

		glBegin (GL_QUADS);
		setTexVertex (vecA, texA.x, texA.y);
		setTexVertex (vecB, texB.x, texB.y);
		setTexVertex (vecC, texC.x, texC.y);
		setTexVertex (vecD, texD.x, texD.y);
		glEnd ();

		if (texturePtr_->isFiltered ())
		{
			texA = Vec2f (0.5 / tWidth, 0.5 / tHeight);
			texB = Vec2f (maxS - 0.5 / tWidth, 0.5 / tHeight);
			texC = Vec2f (maxS - 0.5 / tWidth, maxT - 0.5 / tHeight);
			texD = Vec2f (0.5 / tWidth, maxT - 0.5 / tHeight);

			drawLine (vecA, vecB, texA, texB);
			drawLine (vecB, vecC, texB, texC);
			drawLine (vecC, vecD, texC, texD);
			drawLine (vecD, vecA, texD, texA);
		}
	}
}

void
LfDisplay::drawLine (const Vec2f& point1,
		     const Vec2f& point2) const
{
	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
	glTranslatef (translationVector_.x, translationVector_.y, 0.0f );
	glRotatef (rotationAngle_, 0.0, 0.0, 1.0 );
	glColor4f (drawColor_.r, drawColor_.g, drawColor_.b, drawColor_.a);
	glScalef (scaleFactor_, scaleFactor_, scaleFactor_);
	glBegin (GL_LINES);
	glVertex2f (point1.x, point1.y);
	glVertex2f (point2.x, point2.y);
	glEnd ();
}

void
LfDisplay::drawLine (const Vec2f& point1,
		     const Vec2f& point2,
		     const Vec2f& tex1,
		     const Vec2f& tex2) const
{
	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
	glTranslatef (translationVector_.x, translationVector_.y, 0.0f);
	glRotatef (rotationAngle_, 0.0, 0.0, 1.0);
	glColor4f (drawColor_.r, drawColor_.g, drawColor_.b, drawColor_.a);
	glScalef (scaleFactor_, scaleFactor_, scaleFactor_);
	glBegin (GL_LINES);
	setTexVertex (point1, tex1.x, tex1.y);
	setTexVertex (point2, tex2.x, tex2.y);
	glEnd ();
}


void
LfDisplay::clear () const
{
	glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

void
LfDisplay::flip () const
{
	SDL_GL_SwapBuffers ();
}

LfDisplay::LfDisplay (std::string title,
		      float width,
		      float height,
		      bool fullscreen,
		      int argc,
		      char** argv)
{
	title_ = title;
	width_ = (int) width;
	height_ = (int) height;
	fullscreenFlag_ = fullscreen;
	scaleFactor_ = 1.0f;
	rotationAngle_ = 0.0f;

	SDL_Init (SDL_INIT_VIDEO | SDL_INIT_TIMER);
	SDL_WM_SetCaption (title_.c_str(), NULL);
	SDL_GL_SetAttribute (SDL_GL_RED_SIZE, 5);
	SDL_GL_SetAttribute (SDL_GL_GREEN_SIZE, 6);
	SDL_GL_SetAttribute (SDL_GL_BLUE_SIZE, 5);
	//SDL_GL_SetAttribute (SDL_GL_ALPHA_SIZE, 8);
	SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);

	surfaceLfDisplayPtr_ = SDL_SetVideoMode (width_,
						 height_,
						 0,
						 SDL_OPENGL |
						 SDL_NOFRAME |
						 (fullscreenFlag_ ?
						  SDL_FULLSCREEN :
						  SDL_RESIZABLE));

	if (!surfaceLfDisplayPtr_)
	{
		throw std::runtime_error ("LfDisplay::LfDisplay() - SDL_SetVideoMode() failed");
	}

	glEnable (GL_LINE_SMOOTH);
	glEnable (GL_BLEND);
	glBlendFunc (GL_SRC_ALPHA,
		     GL_ONE_MINUS_SRC_ALPHA);
	glViewport (0, 0, width_, height_);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glOrtho (0.0f, width_, height_, 0.0f, -1.0f, 1.0f);
	glClearColor (0.0f, 0.0f, 0.0f, 1.0f);

#ifndef NO_GL_RECTANGLE_EXTENSION
	std::string exts = (char*) glGetString(GL_EXTENSIONS);

	if (exts.find("GL_NV_texture_rectangle") != std::string::npos)
	{
		// ok
	}
	else if (exts.find("GL_EXT_texture_rectangle") != std::string::npos)
	{
		// ok
	}
	else
	{
		std::cout << "graphics card does not support GL_NV_texture_rectangle or GL_EXT_texture_rectangle\n";
		exit(0);
	}
#endif

	font_ = LfFont ("pixmaps/font-mercedes");
}

void
LfDisplay::setSize (int w,
		    int h)
{
	width_ = w;
	height_ = h;
	glViewport (0, 0, width_, height_);
	glLoadIdentity ();
	glOrtho (0, width_, height_, 0, -1, 1);
}

void
LfDisplay::setBlendMode (BlendMode mode)
{
	switch (mode)
	{
		case normalBlendMode :
			glBlendFunc (GL_SRC_ALPHA,
				     GL_ONE_MINUS_SRC_ALPHA);
			glTexEnvi (GL_TEXTURE_ENV,
				   GL_TEXTURE_ENV_MODE,
				   GL_DECAL);
			glTexEnvi (GL_TEXTURE_ENV,
				   GL_TEXTURE_ENV_COLOR,
				   GL_DECAL);
		break;

		case modulateBlendMode:
			glBlendFunc (GL_DST_COLOR,
				     GL_ZERO);
			glTexEnvi (GL_TEXTURE_ENV,
				   GL_TEXTURE_ENV_MODE,
				   GL_MODULATE);
			glTexEnvi (GL_TEXTURE_ENV,
				   GL_TEXTURE_ENV_COLOR,
				   GL_MODULATE);
		break;

		default:
		break;
	};
}

TexturePtr
LfDisplay::getTexture () const
{
	return (texturePtr_);
}

void
LfDisplay::setWidth (float width)
{
	width_ = (int) width;
}

void
LfDisplay::setHeight (float height)
{
	height_ = (int) height;
}

void
LfDisplay::setFullscreen (bool flag)
{
	fullscreenFlag_ = flag;
}

void
LfDisplay::setTitle (const std::string& title)
{
	title_ = title;
}

// character gaps
static const float hGap (3);
static const float vGap (4);

Vec2f
LfDisplay::getTextSize (const char* text) const
{
	float totalWidth (0);
	const char* p = text;

	if (*p == 0)
		totalWidth = hGap;

	for (; *p; ++p)
	{
		if (*p == '\n')
		{
			Vec2f size (getTextSize (p + 1));
			size += Vec2f (0, font_.getHeight () + vGap);
			if (totalWidth > size.x)
				size.x = totalWidth;
			return size;
		}

		float width = font_.getWidth (*p);
		totalWidth += width + hGap;
	}

	return Vec2f (totalWidth - hGap, font_.getHeight ());
}

void
LfDisplay::drawText (const Vec2f& origin,
		     const std::string& strtext)
{
	const char* text = strtext.c_str ();
	setTexture (font_.getTexture ());

	const float s = 1;

	const float initX (origin.x);
	float x (origin.x);
	float y (origin.y);

	for (; *text; ++text)
	{
		if (*text == '\n')
		{
			x = initX;
			y += font_.getHeight () + vGap;
		}
		else
		{
			float fontWidth (font_.getSCoordWidth (*text));
			float fontHeight (font_.getTCoordHeight ());

			float tx (font_.getSCoordLeft (*text));
			float ty (font_.getTCoordTop (*text));

			const float pixelWidth (font_.getWidth (*text));
			const float pixelHeight (font_.getHeight());

			glBegin (GL_QUADS);
			glTexCoord2f (tx, ty);
			glVertex2f (x, y);

			glTexCoord2f (tx + fontWidth, ty);
			glVertex2f (x + pixelWidth * s, y);

			glTexCoord2f (tx + fontWidth, ty + fontHeight);
			glVertex2f (x + pixelWidth * s, y + pixelHeight * s);

			glTexCoord2f (tx, ty + fontHeight);
			glVertex2f (x, y + pixelHeight * s);

			glEnd ();

			x += pixelWidth + hGap;
		}
	}
}
