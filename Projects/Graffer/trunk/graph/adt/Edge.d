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

module graph.adt.Edge;

private import 
	graph.adt.Vertex, 
	cairo.Context;

/**
	\brief	An Edge, Arc or Connection in a Graph.
	
	Edge is a straight line representing a pair of Vertex, connecting them 
	together.
 */
struct Edge {
public:
	Vertex*	v1;			/**< The first Vertex that Edge is connected to. */
	Vertex*	v2;			/**< The second Vertex that Edge is connected to. */
	bool	selected;	/**< Highlight if intersecting other edge. */

	/**
		\brief	Expose event method to draw the Edge.
		
		Moves to the position of Vertex v1, draws a straight line to the 
		position of Vertex v2 and flushes the changes to a Cairo Context.
		
		\param	c is a reference of the Cairo Context to render the Edge to.
	*/
	void expose(ref Context c) {
		c.save;
		
		if (selected)
			c.setSourceRgb (0.8, 0.0, 0.0);
			
		c.moveTo(v1.pos.x, v1.pos.y);
		c.lineTo(v2.pos.x, v2.pos.y);
		c.stroke;
		c.restore;
	}
	
	/**
	 * Calculates the slope of the line representing the edge.
	 * 
	 * Return: Value of the slope of the line representing the edge.
	 */
	double slope () {
		return (v1.pos.y - v2.pos.y) / (v1.pos.x - v2.pos.x);
	}
	
	/**
	 * Calculates the constant in the line equation y = constant + (slope)(x)
	 * 
	 * Return: Constant part of the line equation.
	 */
	double constant () {
		return v1.pos.y - (slope * v1.pos.x);
	}
	
	/**
	 * Checks if point (x, y) lies on the line representing edge.
	 * 
	 * Return: True if point lies and false otherwise.
	 */
	bool lies (double x, double y) {
		if ((v1.pos.x - x) * (x - v2.pos.x) >= 0 &&
			(v1.pos.y - y) * (y - v2.pos.y) >= 0)
			return true;
		else
			return false;
	}
}
