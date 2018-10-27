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

module graph.adt.Vertex;

private import
	graph.adt.Vector,
	cairo.Context,
	tango.math.Math;

/**
	\brief	A Vertex or Node.
	
	Represents a Vertex or Node in a Graph.
 */
struct Vertex {
public:
	Vector	pos,				/**< Position of Vertex. */
			vel,				/**< Velocity of Vertex. */
			netf;				/**< Netforce on Vertex. */
	int		size,				/**< Repulsion size of Vertex. */
			drawSize;			/**< Drawing size of Vertex. */
	bool	locked = false,		/**< Vertex is locked or not. */
			selected = false;	/**< Vertex is selected or not. */

	/**
		\brief	Expose event method to draw the Vertex.
		
		Saves the current state of the Cairo Context's properties. If the Vertex 
		is locked, set the color to red. Then, render a circular arc and fill 
		it and restore the Cairo Context's settings.
		
		\param	c is a reference of the Cairo Context to render the Vertex to.
	 */
	void expose(ref Context c) {
		c.save();
		
		if (selected) {
			c.save();
			c.setSourceRgb(0.9, 0.6, 0.0);
			c.rectangle(pos.x - drawSize - 10, pos.y - drawSize - 10, drawSize * 2 + 20, drawSize * 2 + 20);
			c.fillPreserve();
			c.setSourceRgb(0.7, 0.4, 0.0);
			c.stroke();
			c.restore();
		}

		if (locked)
			c.setSourceRgb(0.8, 0.0, 0.0);

		c.arc(pos.x, pos.y, drawSize, 0, 2 * PI);
		c.closePath();
		c.fill();
		c.restore();
	}
}
