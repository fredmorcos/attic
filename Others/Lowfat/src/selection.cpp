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
#include "include/selection.h"

SelectionPtr
Selection::make ()
{
	return SelectionPtr (new Selection ());
}

Selection::Selection ()
	: start_ (0.0f, 0.0f),
	  end_ (0.0f, 0.0f)
{
}

Selection::~Selection ()
{
}

void
Selection::paint (LfDisplay& display)
{
	Vec2f start = start_;
	Vec2f end = end_;

	if (start_.x > end_.x)
	{
		start.x = end_.x;
		end.x = start_.x;
	}

	if (start_.y > end_.y)
	{
		start.y = end_.y;
		end.y = start_.y;
	}

	display.setTexture (TexturePtr (0));
	display.setDrawColor (Color (0.9f, 0.9f, 1.0f, 0.1f));
	display.drawRectangleMapped (start.x,
				     start.y,
				     end.x - start.x,
				     end.y - start.y,
				     0);

	Vec2f tr = end;
	tr.y = start.y;
	Vec2f bl = end;
	bl.x = start.x;

	display.setDrawColor (Color (1.0f, 1.0f, 1.0f, 1.0f));
	display.drawLine (start, tr);
	display.drawLine (tr, end);
	display.drawLine (end, bl);
	display.drawLine (bl, start);
}

void
Selection::setStart (Vec2f start)
{
	start_ = start;
}

void
Selection::setEnd (Vec2f end)
{
	end_ = end;
}

Vec2f
Selection::getStart () const
{
	return start_;
}

Vec2f
Selection::getEnd () const
{
	return end_;
}
