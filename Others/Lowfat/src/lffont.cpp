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

#include <fstream>
#include <iostream>

#include "include/lffont.h"
#include "include/texture.h"

LfFont::LfFont()
{
}

LfFont::LfFont (const std::string& name)
{
	std::ifstream file ((name + ".metrics").c_str ());

	file >> height_;
	pixelHeight_ = height_;

	fontTexture_ = Texture::make (name + ".tga");

	const float texWidth (fontTexture_->getWidth ());
	const float texHeight (fontTexture_->getHeight ());

	height_ /= texHeight;
	maxWidth_ = 0.0f;

	while (!file.eof ())
	{
		int idx;
		float x;
		float y;
		float width;

		file >> idx;
		file >> x;
		file >> y;
		file >> width;

		pixelWidth_[idx] = width;
		if (width > maxWidth_)
			maxWidth_ = width;

		x /= texWidth + 1.0 / (texWidth * 2);
		y /= texHeight + 1.0 / (texHeight * 2);
		--width;
		width /= texWidth;

		x_[idx] = x;
		y_[idx] = y;
		width_[idx] = width;
	}
}

LfFont::~LfFont ()
{
}

float
LfFont::getSCoordLeft (int i) const
{
	return x_[i];
}

float
LfFont::getTCoordTop (int i) const
{
	return y_[i];
}

float
LfFont::getSCoordRight (int i) const
{
	return x_[i] + width_[i];
}

float
LfFont::getTCoordBottom (int i) const
{
	return y_[i] + height_;
}

float
LfFont::getSCoordWidth (int i) const
{
	return width_[i];
}

float
LfFont::getTCoordHeight () const
{
	return height_;
}

float
LfFont::getWidth (int i) const
{
	return pixelWidth_[i];
}

float
LfFont::getHeight () const
{
	return pixelHeight_;
}

float
LfFont::getMaxWidth () const
{
	return maxWidth_;
}

TexturePtr
LfFont::getTexture () const
{
	return fontTexture_;
}

