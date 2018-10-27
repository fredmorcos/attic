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

#include <math.h>
#include <stdexcept>
#include <strstream>
#include <vector>
#include <GL/gl.h>
#include "SDL.h"
#include "SDL_image.h"
#include "include/texture.h"

// nasty
#ifndef GL_TEXTURE_RECTANGLE_EXT
#define GL_TEXTURE_RECTANGLE_EXT 0x84F5
#endif

#ifndef GL_CLAMP_TO_EDGE
#define GL_CLAMP_TO_EDGE 0x812F 
#endif

static int
nextPowerOfTwo (int x)
{
	int n (1);

	while (x > n)
		n <<= 1;

	return n;
}

static bool
isPowerOfTwo (int x)
{
	if (x == 0)
		return false;
	else
		return ((x - 1) & x) == 0;
}

TexturePtr
Texture::make (std::string filename,
	       bool filter)
{
	return TexturePtr (new Texture (filename, 1.0f, 1.0f, filter));
}

TexturePtr
Texture::make (std::string filename,
	       float maxSCoord,
	       float maxTCoord,
	       bool filter)
{
	return TexturePtr (new Texture (filename,
					maxSCoord,
					maxTCoord,
					filter));
}

Texture::~Texture ()
{
	int level = 0;

	while (mipmap_[level].init)
	{
		glDeleteTextures (1, &(mipmap_[level].textureId));
		level++;
	}
}

float
Texture::getWidth () const
{
	return (mipmap_[mipMapLevel_].width);
}

float
Texture::getHeight () const
{
	return (mipmap_[mipMapLevel_].height);
}

unsigned int
Texture::getTextureId () const
{
	return (mipmap_[mipMapLevel_].textureId);
}

unsigned int
Texture::getTextureTarget () const
{
	return (textureTarget_);
}

void
Texture::setTextureId (unsigned int textureId)
{
	textureId_ = textureId;
}

void
Texture::setTextureTarget (unsigned int textureTarget)
{
	textureTarget_ = textureTarget;
}

Texture::Texture (std::string filename,
		  float maxSCoord,
		  float maxTCoord,
		  bool filter)
{
	mipMapLevel_ = 0;
	isFiltered_ = filter;
	SDL_Surface* surfaceImage;
	filename_ = filename;

	std::cout << "IMG_LOAD "
		  << filename
		  << "... ";

	surfaceImage = IMG_Load (filename_.c_str ());
	std::cout << "finished\n";

	if (!surfaceImage)
	{
		std::strstream str;
		str << "couldn't not open image file \"" << filename << "\"" << '\0';
		throw std::runtime_error(str.str());
	}
	else
	{
		width_  = surfaceImage->w;
		height_ = surfaceImage->h;

#ifndef NO_GL_RECTANGLE_EXTENSION
		textureTarget_ = GL_TEXTURE_RECTANGLE_EXT;
#else
		textureTarget_ = GL_TEXTURE_2D;
#endif
		upload (reinterpret_cast<const char*>(surfaceImage->pixels),
			surfaceImage->w,
			surfaceImage->pitch,
			surfaceImage->h,
			surfaceImage->format->BytesPerPixel,
			filter);

		SDL_FreeSurface (surfaceImage);
	}
}

void
Texture::upload (const char* bitmap,
		 int width,
		 int pitch,
		 int height,
		 int bytesPerPixel,
		 bool filter)
{
	int format;

	switch (bytesPerPixel)
	{
		case 3:
			format = GL_RGB;
			hasAlpha_ = false;
		break;

		case 4:
			format = GL_RGBA;
			hasAlpha_ = true;
		break;

		default:
			throw std::runtime_error ("unsupported image format");
		break;
	};

	void* pixels = (void*) bitmap;

	float actualWidth;
	float actualHeight;
	int powWidth (nextPowerOfTwo (width_));
	int powHeight (nextPowerOfTwo (height_));

	std::vector<char> newImage;
	const int bpp (bytesPerPixel);

	if (!isPowerOfTwo (width_) ||
	    !isPowerOfTwo (height_))
	{
		newImage.resize (powWidth * bpp * powHeight);

		for (int y = 0; y < height_; ++y)
		{
			char* dst = &newImage[bpp * y * powWidth];
			const char* src = &(bitmap)[y * pitch];
			memcpy (dst, src, width_ * bpp);
		}

		if (powWidth > width)
			for (int y = 0; y < height_; ++y)
				for (int i = 0; i < bpp; ++i)
					newImage[bpp * y * powWidth + bpp * width + i] = bitmap[y * pitch + bpp * (width - 1) + i];

		if (powHeight > height)
			for (int x = 0; x < width_; ++x)
				for (int i = 0; i < bpp; ++i)
					newImage[bpp * height * powWidth + x * bpp + i] = bitmap[x * bpp + bpp * (height - 1) * width + i];

		if (powWidth > width && powHeight > height)
			for (int i = 0; i < bpp; ++i)
				newImage[bpp * height * powWidth + bpp * width + i] = bitmap[bpp * (width - 1) + bpp * (height - 1) * width + i];

		std::cout << "...finished\n";

		pixels = &newImage[0];
		maxSCoord_ = (float) (width_) / (powWidth);
		maxTCoord_ = (float) (height_) / (powHeight);

		actualWidth = powWidth;
		actualHeight = powHeight;
	}
	else
	{
		actualWidth = width;
		actualHeight = height;
	}

	int level = 0;
	int maxLevel = 32;

	if (!filter)
		maxLevel = 1;

	unsigned char* image = (unsigned char*) pixels;

	while ((level < maxLevel) &&
	       (powWidth >= 1) &&
	       (powHeight >= 1))
	{
		mipmap_[level].width = width;
		mipmap_[level].height = height;
		mipmap_[level].maxSCoord = (float) width / powWidth;
		mipmap_[level].maxTCoord = (float) height / powHeight;
		mipmap_[level].init = true;

		glGenTextures (1, &(mipmap_[level].textureId));
		glBindTexture (textureTarget_, mipmap_[level].textureId);
		glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
		glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
		glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, GL_DECAL);

		if (textureTarget_ == GL_TEXTURE_2D)
		{
			if (filter)
			{
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MIN_FILTER,
						 GL_LINEAR );
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MAG_FILTER,
						 GL_LINEAR);
			}
			else
			{
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MIN_FILTER,
						 GL_NEAREST);
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MAG_FILTER,
						 GL_NEAREST);
			}

			glTexParameteri (textureTarget_,
					 GL_TEXTURE_WRAP_S,
					 GL_CLAMP_TO_EDGE);
			glTexParameteri (textureTarget_,
					 GL_TEXTURE_WRAP_T,
					 GL_CLAMP_TO_EDGE);
			}
			else
			{
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MAG_FILTER,
						 GL_LINEAR);
				glTexParameteri (textureTarget_,
						 GL_TEXTURE_MIN_FILTER,
						 GL_LINEAR);
				maxSCoord_ = width_;
				maxTCoord_ = height_;
			}

			glTexImage2D (textureTarget_,
				      0,
				      format,
				      powWidth,
				      powHeight,
				      0,
				      format,
				      GL_UNSIGNED_BYTE,
				      pixels);

			if ((level < maxLevel) &&
			    (powWidth >= 1) &&
			    (powHeight >= 1))
			{
				for (int y = 0; y < powHeight / 2; y++)
				{
					for (int x = 0; x < powWidth / 2; x++)
					{
						for (int i = 0; i < bpp; ++i)
						{
							float accu = 0;
							float accu1 = image[bpp * ((y*2+0) * powWidth + (x*2+0)) + i] / 255.0;
							float accu2 = image[bpp * ((y*2+0) * powWidth + (x*2+1)) + i] / 255.0;
							float accu3 = image[bpp * ((y*2+1) * powWidth + (x*2+0)) + i] / 255.0;
							float accu4 = image[bpp * ((y*2+1) * powWidth + (x*2+1)) + i] / 255.0;

							accu = accu1 * 255;
							accu += accu2 * 255;
							accu += accu3 * 255;
							accu += accu4 * 255;
							accu /= 4;

							if (accu > 255)
								accu = 255;

							if (accu < 0)
								accu = 0;

							image[bpp * (y * (powWidth / 2) + x) + i] = (unsigned char) accu;
					}
				}
			}
		}

		width /= 2;
		height /= 2;
		powWidth /= 2;
		powHeight /= 2;
		level++;
	}
}

std::string
Texture::getFileName () const
{
	return (filename_);
}

void
Texture::setFileName (std::string filename)
{
	filename_ = filename;
}

float
Texture::getMaxS () const
{
	return (mipmap_[mipMapLevel_].maxSCoord);
}

void
Texture::setMaxS (float maxSCoord)
{
	maxSCoord_ = maxSCoord;
}

float
Texture::getMaxT () const
{
	return (mipmap_[mipMapLevel_].maxTCoord);
}

void
Texture::setMaxT (float maxTCoord)
{
	maxTCoord_ = maxTCoord;
}

bool
Texture::hasAlpha () const
{
	return( hasAlpha_ );
}

void
Texture::setWidth (float width)
{
	width_ = (int) width;
}

void
Texture::setHeight (float height)
{
	height_ = (int) height;
}

void
Texture::setAlpha (bool alphaFlag)
{
	hasAlpha_ = alphaFlag;
}

bool
Texture::isFiltered () const
{
	return isFiltered_;
}

void
Texture::setMipMapLevel (int level)
{
	int maxLevel = 0;

	while (mipmap_[maxLevel].init)
		maxLevel++;

	maxLevel--;

	if (level > maxLevel)
		level = maxLevel;

	mipMapLevel_ = level;
}

