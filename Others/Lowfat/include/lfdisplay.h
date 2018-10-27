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

#ifndef _LFDISPLAY_H
#define _LFDISPLAY_H

#include <string>

struct SDL_Surface;

#include "include/shared_ptr.h"
#include "include/texture.h"
#include "include/color.h"
#include "include/lfmath.h"
#include "include/lffont.h"

class LfDisplay;
typedef SharedPtr<LfDisplay> LfDisplayPtr;

class LfDisplay
{
	public:
		~LfDisplay();
		int getWidth () const;
		int getHeight () const;
		bool isFullscreen () const;
		std::string getTitle () const;
		float getScaleFactor () const;
		void setScaleFactor (float scaleFactor);
		Vec2f getTranslation () const;
		void setTranslation (const Vec2f& translationVector);
		void setTranslation (float vecPartX, float vecPartY);
		float getRotation () const;
		void setRotation (float rotationAngle);
		void setTexture (TexturePtr texture, float size = 1.0f);
		Color getDrawColor () const;
		void setDrawColor (const Color& drawColor);
		void setDrawColor (float r, float g, float b, float a);
		void drawRectangleMapped (float x,
					  float y,
					  float width,
					  float height,
					  float angle = 0);

		void drawQuad (const Vec2f& vec1,
			       const Vec2f& vec2,
			       const Vec2f& vec3,
			       const Vec2f& vec4) const;

		void drawQuad (const Vec2f& vec1,
			       const Color& col1,
			       const Vec2f& vec2,
			       const Color& col2,
			       const Vec2f& vec3,
			       const Color& col3,
			       const Vec2f& vec4,
			       const Color& col4) const;

		void drawLine (const Vec2f& point1,
			       const Vec2f& point2) const;

		void drawRectWrapped (float x,
				      float y,
				      float width,
				      float height);

		void drawLine (const Vec2f& point1,
			       const Vec2f& point2,
			       const Vec2f& tex1,
			       const Vec2f& tex2) const;

		void clear () const;
		void flip () const;

		enum BlendMode
		{
			normalBlendMode,
			modulateBlendMode
		};

		void setBlendMode (BlendMode mode);
		Vec2f getTextSize (const char* text) const;

		void setSize (int w, int h);

		static LfDisplayPtr make (std::string title,
					  float width,
					  float height,
					  bool fullscreenFlag,
					  int argc = 0,
					  char** argv = NULL);

		void drawText (const Vec2f&, const std::string& text);

	private:
		LfDisplay (std::string title,
			   float width,
			   float height,
			   bool fullscreen,
			   int argc = 0,
			   char** argv = NULL);

		TexturePtr getTexture () const;
		void setWidth (float width);
		void setHeight (float height);
		void setFullscreen (bool flag);
		void setTitle (const std::string& title);

		LfFont		font_;
		SDL_Surface*	surfaceLfDisplayPtr_;
		int		width_;
		int		height_;
		bool		fullscreenFlag_;
		std::string	title_;
		float		scaleFactor_;
		Vec2f		translationVector_;
		float		rotationAngle_;
		TexturePtr	texturePtr_;
		Color		drawColor_;
};

#endif  // _LFDISPLAY_H
