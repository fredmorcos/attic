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

#include "SDL_image.h"

#include "include/lfmath.h"
#include "include/lfdisplay.h"
#include "include/background.h"

BackgroundPtr
Background::make (const std::string& background,
		  const std::string& backgroundLight)
{
	return BackgroundPtr (new Background (background, backgroundLight));
}

Background::Background (const std::string& background,
			const std::string& backgroundLight) :
    background_ (Texture::make (background, false)),
    backgroundLight_ (Texture::make (backgroundLight, true))
{
}

Background::~Background ()
{
}

void
Background::paint (LfDisplay& display)
{
	if (!background_.isNull ())
	{
		display.setTexture (background_);
		display.setDrawColor (Color::white);

		const int bwidth  = (int) background_->getWidth ();
		const int bheight = (int) background_->getHeight ();

		int startX = (int) ((display.getWidth () - bwidth) / 2.0f);
		int startY = (int) ((display.getHeight () - bheight) / 2.0f);
		while (startX > 0)
		{
			startX -= bwidth;
		}
		while (startY > 0)
		{
			startY -= bheight;
		}

		// draw main bg-texture streched
		display.drawRectWrapped (0,
					 0,
					 display.getWidth (),
					 display.getHeight ());

		if (!backgroundLight_.isNull ())
		{
			display.setDrawColor (Color::white);
			display.setTexture (backgroundLight_);
			display.setBlendMode (LfDisplay::modulateBlendMode);
			display.drawRectWrapped (0,
						 0,
						 display.getWidth (),
						 display.getHeight ());
			display.setBlendMode (LfDisplay::normalBlendMode);
		}
	}
}

bool
Background::isInBoundingBox (int x,
			     int y) const
{
	return true;
}
