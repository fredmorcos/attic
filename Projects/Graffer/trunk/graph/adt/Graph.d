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

module graph.adt.Graph;

private import 
	graph.adt.Vertex, 
	graph.adt.Edge,
	graph.adt.Quadrant,
	cairo.Context,
	tango.math.Math;

/**
	\brief	Templates of the flow of Edges in a Graph.
	
	Represents information about the way Edges will be connected to Vertices in 
	a Graph, forming a certain shape.
 */
public enum GraphShape {
	NONE,
	CIRCULAR,
	CENTERED,
	INTERCONNECTED,
	BINARYTREE
}

/**
	\brief	A Graph data structure.
	
	A set of Edge and Vertex, representing a Graph.
 */
struct Graph {
public:
	Vertex*[]	vertices;	/**< Set of Vertex. */
	Edge*[]		edges;		/**< Set of Edge. */
	int			width,		/**< Graph frame width. */
				height;		/**< Graph frame height. */
	bool		wall;		/**< Effect of wall enabled or disabled. */

	/**
		\brief	Get the Vertex at position (x, y).
		
		Looks up and returns the Vertex at position (x, y). Compares x and y to 
		the drawing boundaries of each Vertex in the list of Vertex. The first 
		Vertex to match the position is returned.
		
		\param	x is the X value of the position to lookup a Vertex at.
		\param	y is the Y value of the position to lookup a Vertex at.
		\return	A pointer to the Vertex at position (x, y).
	 */
	Vertex* getVertexByPosition(double x, double y) {
		foreach(ref v; vertices)
			if(x >= v.pos.x - v.drawSize && x <= v.pos.x + v.drawSize && 
					y >= v.pos.y - v.drawSize && y <= v.pos.y + v.drawSize)
				return v;
		return null;
	}

	/**
		\brief	Shift all Vertex positions by (x, y).
		
		Iterates over all vertices and shifts each position by (x, y).
		
		\param	x is the shift value in the X direction.
		\param	y is the shift value in the Y direction.
	 */
	void shiftVerticesBy(double x, double y) {
		if (x == 0.0 && y == 0.0)
			return;

		foreach(ref v; vertices) {
			v.pos.x += x;
			v.pos.y += y;
		}
	}

	/**
		\brief	Returns a quadrant around the graph.
		
		Finds the minimum and maximum positions of vertices in the graph 
		and returns a square Quadrant representing the exact size of the graph.
		
		\return a Quadrant representing a square around the graph.
	 */
	void getQuadrant(ref Quadrant q) {
		if (!vertices.length) {
			q.x = 0;
			q.y = 0;
			q.length = 0;
			return;
		}
			
		double		minx = vertices[0].pos.x,
					miny = vertices[0].pos.x,
					maxx = vertices[0].pos.x,
					maxy = vertices[0].pos.x;

		foreach(ref v; vertices) {
			if (v.pos.x < minx) minx = v.pos.x;
			if (v.pos.y < miny)	miny = v.pos.y;
			if (v.pos.x > maxx)	maxx = v.pos.x;
			if (v.pos.y > maxy) maxy = v.pos.y;
		}
		
		q.x = minx - 20;
		q.y = miny - 20;
		q.length = cast(int) max(maxx - minx, maxy - miny) + 40;
	}

	/**
		\brief	Expose event method to draw the Graph.
		
		If the wall is enabled, will save the current state of the Cairo 
		Context's properties, draw the wall with red color and restore the 
		Cairo Context's state back. Then, will iterate over the list of Edge, 
		render the edges, iterate over the list of Vertex and render the 
		vertices.
		
		\param	c is a reference of the Cairo Context to render the Graph to.
	 */
	void expose(ref Context c) {
		if (wall) {
			c.save();
			c.setSourceRgb(0.8, 0.0, 0.0);
			c.setLineWidth(10.0);
			c.rectangle(0, 0, width, height);
			c.stroke();
			c.restore();
		}
		foreach(ref e; edges)
			e.expose(c);
		foreach(ref v; vertices)
			v.expose(c);
	}
	
	/**
		\brief	Returns the index of Vertex v in the vertices list.
		
		\param	v is the Vertex to lookup its index in the vertices list.
		\return Index of v in the vertices list.
	 */
	int getVertexIndex(Vertex *v) {
		foreach(i, ver; vertices)
			if(ver == v)
				return i;
		return -1;
	}
}
