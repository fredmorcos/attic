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

#include "include/component.h"

Component::Component () : 
    location_ (Vec2f (0.0f, 0.0f)),
    size_ (Vec2f (1.0f, 1.0f))
{
}

Component::~Component ()
{
}

Vec2f
Component::getLocation () const
{
	return location_;
}

Vec2f
Component::getSize () const
{
	return size_;
}

void
Component::setX (float x)
{
	location_.x = x;
}

void
Component::setY (float y)
{
	location_.y = y;
}

void
Component::setLocation (const Vec2f& location)
{
	if ((location.x != location_.x) ||
	    (location.y != location_.y))
	{
		location_ = location;
	}
}

void
Component::setSize (const Vec2f& size)
{
	size_ = size;
}

void
Component::setLocation (float x,
			float y)
{
	if ((x != location_.x) || (y != location_.y))
		setLocation (Vec2f(x, y));
}

void
Component::setSize (float w,
		    float h)
{
	setSize (Vec2f (w, h));
}

float
Component::getWidth () const
{
	return size_.x;
}

float Component::getHeight () const
{
	return size_.y;
}

float Component::getX () const
{
	return location_.x;
}

float Component::getY () const
{
	return location_.y;
}

void Component::advance (unsigned int t)
{
}

void
Component::getBoundingBox (Vec2f& topLeft,
			   Vec2f& bottomRight)
{
	topLeft = Vec2f (0.0f, 0.0f);
	bottomRight = size_;
}

int
Component::intersectBox (const Vec2f topLeft,
			 const Vec2f bottomRight) const
{
	return -1;
}

bool
Component::isInside (int x,
		     int y) const
{
	return	x >= getX () - getWidth ()  / 2.0f &&
		x < getX ()  + getWidth ()  / 2.0f &&
		y >= getY () - getHeight () / 2.0f &&
		y < getY ()  + getHeight () / 2.0f;
}

