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

#ifndef _SELECTION_H
#define _SELECTION_H

#include "include/component.h"
#include "include/forward.h"

#include <vector>

class LfDisplay;

class Selection : public Component
{
	public:
		static SelectionPtr make ();

		virtual ~Selection ();
		virtual void paint (LfDisplay& display);

		void	setStart (Vec2f start);
		void	setEnd (Vec2f end);

		Vec2f	getStart () const;
		Vec2f	getEnd () const;

	protected:
		Selection ();

		Vec2f start_;
		Vec2f end_;
};

#endif // _SELECTION_H
