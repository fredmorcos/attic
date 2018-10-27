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

#ifndef _LFFONT_H
#define _LFFONT_H

#include <string>

#include "include/forward.h"
#include "include/texture.h"

class LfFont
{
	enum
	{
		charCount = 256
	};

	float x_[charCount];
	float y_[charCount];
	float width_[charCount];
	float height_;
	float pixelWidth_[charCount];
	float pixelHeight_;
	float maxWidth_;
	TexturePtr fontTexture_;

	public:
		LfFont ();
		LfFont (const std::string& name);
		~LfFont ();

		float getSCoordLeft (int i) const;
		float getTCoordTop (int i) const;
		float getSCoordRight (int i) const;
		float getTCoordBottom (int i) const;
		float getSCoordWidth (int i) const;
		float getTCoordHeight () const;
		float getWidth (int i) const;
		float getHeight () const;
		float getMaxWidth () const;
		TexturePtr getTexture () const;
};

#endif // _LFFONT_H
