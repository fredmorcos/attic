/*
	This file is part of Grafer.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer.  If not, see <http://www.gnu.org/licenses/>.
*/

module graph.adt.Quadrant;

/**
	\brief	A Quadrant data structure.

	Represents a quadrant structure on a plane.
 */
struct Quadrant {
	double	x,			/**< X position of the upper left corner. */
			y;			/**< Y position of the upper left corner. */
	int		length;		/**< Length of a side of a Quadrant. */
	
	double getCenterX() {
		return x + (length / 2);
	}
	
	double getCenterY() {
		return y + (length / 2);
	}
}
