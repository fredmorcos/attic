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

#ifndef _COMPONENT_H
#define _COMPONENT_H

#include "include/forward.h"
#include "include/lfmath.h"
#include "include/shared_ptr.h"

class Component;
class LfDisplay;

typedef SharedPtr<Component> ComponentPtr;

class Component
{
	public:
		virtual ~Component ();
		virtual void paint (LfDisplay& display) = 0;
		virtual Vec2f getLocation () const;
		virtual Vec2f getSize () const;
		virtual float getWidth () const;
		virtual float getHeight () const;
		virtual void setLocation (const Vec2f& location);
		virtual void setLocation (float x, float y);
		virtual void setSize (const Vec2f& size);
		virtual void setSize (float w, float h);
		virtual float getX () const;
		virtual float getY () const;
		virtual void advance (unsigned int);
		virtual void getBoundingBox (Vec2f& topLeft,
					     Vec2f& bottomRight);
		virtual int intersectBox (const Vec2f topLeft,
					  const Vec2f bottomRight) const;
		virtual bool isInside (int x, int y) const;
		void setX (float x);
		void setY (float y);

	protected:
		Component ();

	private:
		Vec2f location_;
		Vec2f size_;
};

#endif // _COMPONENT_H
