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

#ifndef _COLOR_H
#define _COLOR_H

struct Color
{
	public:
		float r;
		float g;
		float b;
		float a;

		Color () : r(0), g(0), b(0), a(1)
		{
		}

		Color (float nr,
		       float ng,
		       float nb,
		       float na = 1) : r(nr), g(ng), b(nb), a(na)
		{
		}

		Color& set (float nr,
			    float ng,
			    float nb,
			    float na)
		{
			r = nr;
			g = ng;
			b = nb;
			a = na;

			return *this;
		}

		static const Color white;
		static const Color black;
		static const Color red;
		static const Color green;
		static const Color blue;
		static const Color yellow;
		static const Color brown;
		static const Color lightGray;
		static const Color gray;
		static const Color darkGray;
		static const Color pink;
		static const Color orange;
		static const Color magenta;
		static const Color cyan;
};

#endif // _COLOR_H
