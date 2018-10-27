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

#ifndef _TEXTURE_H
#define _TEXTURE_H

#include <string>

#include "include/shared_ptr.h"

#define NO_GL_RECTANGLE_EXTENSION

class LfDisplay;
class Texture;
typedef SharedPtr<Texture> TexturePtr;

class Texture
{
	class MipMap
	{
		public:
			MipMap ()
			{
				init = false;
			}
			int		width;
			int		height;
			unsigned int	textureId;
			float		maxSCoord;
			float		maxTCoord;
			bool		init;
	};


    friend class LfDisplay;

	public:
		static TexturePtr make (std::string filename, bool filter = true);
		static TexturePtr make (std::string filename,
					float maxSCoord,
					float maxTCoord,
					bool filter = true);
		~Texture ();
		float getWidth () const;
		float getHeight () const;
		unsigned int getTextureId () const;
		unsigned int getTextureTarget () const;
		void setTextureId (unsigned int textureId);
		void setTextureTarget (unsigned int textureTarget);
		float getMaxS () const;
		float getMaxT () const;
		void setMaxS (float maxSCoord);
		void setMaxT (float maxTCoord);
		bool hasAlpha () const;
		bool isFiltered () const;

		MipMap mipmap_[32];
		void setMipMapLevel (int level);

	private:
		void upload (const char* bitmap,
			     int width,
			     int pitch,
			     int height,
			     int bytesPerPixel,
			     bool filter);

		Texture (std::string filename,
			 float maxSCoord,
			 float maxTCoord,
			 bool filter = false);

		std::string getFileName () const;
		void setFileName (std::string filename);
		void setWidth (float width);
		void setHeight (float height);
		void setAlpha (bool alphaFlag);

		std::string filename_;
		int width_;
		int height_;
		unsigned int textureId_;
		unsigned int textureTarget_;
		float maxSCoord_;
		float maxTCoord_;
		bool hasAlpha_;
		bool isFiltered_;
		int mipMapLevel_;
};

#endif  // _TEXTURE_H
