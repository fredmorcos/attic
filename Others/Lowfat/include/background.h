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

#ifndef _BACKGROUND_H
#define _BACKGROUND_H

#include "include/component.h"
#include "include/forward.h"
#include "include/texture.h"

#include <vector>

class LfDisplay;

class Background : public Component
{
	public:
		static BackgroundPtr make (const std::string& background,
					   const std::string& backgroundLight);

		virtual ~Background ();
		virtual void paint (LfDisplay& display);
		virtual bool isInBoundingBox (int x, int y) const;

	protected:
		Background (const std::string& background,
			    const std::string& backgroundLight);
		Background () {};

	private:
		TexturePtr background_;
		TexturePtr backgroundLight_;
};

#endif // _BACKGROUND_H
