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
	cairo.Context,
	graph.adt.interfaces.Exportable,
	graph.adt.interfaces.LayoutableGraph,
	graph.adt.interfaces.Renderable,
	GraphIF = graph.adt.interfaces.Graph,
	tango.text.xml.Document,
	Integer = tango.text.convert.Integer;

/**
 * A set of Nodes and a set of Edges representing a Graph structure. X, Y, Width 
 * and Height represent the boundaries of the Graph.
 * 
 * Implements the Graph (and Boundary), LayoutableGraph and Renderable 
 * interfaces.
 */
class Graph(N, E):	
	GraphIF.Graph!(N, E),
	LayoutableGraph,
	Renderable,
	Exportable {
	
	private:
	
	int		x_,			/** X position of the Graph. See Boundary Interface. */
			y_,			/** Y position of the Graph. See Boundary Interface. */
			width_,		/** Width of the Graph. See Boundary Interface. */
			height_;	/** Height of the Graph. See Boundary Interface. */
	N[]		nodes_;		/** Set of Nodes in the Graph. See Graph Interface. */
	E[]		edges_;		/** Set of Edges in the Graph. See Graph Interface. */
	bool	wall_;		/** Wall of the Graph. See LayoutableGraph Interface. */
	
	public:
	
	/**
	 * Finds the minimum and maximum positions of Nodes in the Graph and updates 
	 * the Graph's X, Y, Width and Height values. Also updates the Node IDs.
	 */
	void update () {
		if (!nodes_.length) {
			x_ = 0;
			y_ = 0;
			width_ = 0;
			height_ = 0;
			return;
		}
		
		int	minx = nodes_[0].x - nodes_[0].width,
			miny = nodes_[0].y - nodes_[0].height,
			maxx = nodes_[0].x + nodes_[0].width,
			maxy = nodes_[0].y + nodes_[0].height;
		
		if (nodes_.length == 1) {
			nodes_[0].id = 0;
			return;
		}

		foreach(i, ref n; nodes_) {
			nodes_[i].id = i;
			if (n.x - n.width	< minx) minx = n.x - n.width;
			if (n.y - n.height	< miny) miny = n.y - n.height;
			if (n.x + n.width	> maxx)	maxx = n.x + n.width;
			if (n.y + n.height	> maxy) maxy = n.y + n.height;
		}
		
		x_ = minx;
		y_ = miny;
		width_ = maxx - minx;
		height_ = maxy - miny;
	}
	
	/**
	 * Looks up and returns the Node at position (x, y). Compares x and y to 
	 * the boundaries of each Node in the list of Nodes. The first Node to match 
	 * the position is returned.
	 * 
	 * Params:
	 * 		x = is the X value of the position to lookup a Node at.
	 * 		y = is the Y value of the position to lookup a Node at.
	 * 
	 * Returns:
	 * 		The Node at position (x, y).
	 */
	Node getNodeByPosition(int x, int y) {
		foreach(ref n; nodes_)
			if(x >= n.x - n.width && x <= n.x + n.width && 
					y >= n.y - n.height && y <= n.y + n.height)
				return n;
		return null;
	}
	
	/**
	 * Iterates over all Nodes and shifts each Node's position by (x, y).
	 * 
	 * Params:
	 * 		x = is the shifting value in the X direction.
	 * 		y = is the shifting value in the Y direction.
	 */
	void moveNodesBy(double x, double y) {
		if (x == 0.0 && y == 0.0)
			return;

		foreach(ref n; nodes_) {
			v.x += x;
			v.y += y;
		}
	}
	
	/**
	 * Returns the index of Node n in the Nodes list.
	 * 
	 * Params:
	 * 		n = is the Node to lookup in the Nodes list.
	 * 
	 * Returns:
	 * 		The index of n in the Nodes list.
	 */
	int getVertexIndex(N n) {
		foreach(i, ref node; nodes_)
			if(node == n)
				return i;
		return -1;
	}
	
	/**
	 * Renders the Graph to a Cairo Context. If the wall is enabled, will save 
	 * the current state of the Cairo Context's properties, draw the wall with 
	 * red color and restore the Cairo Context's state back. Then, will iterate 
	 * over the list of Edges and render them then iterate over the list of 
	 * Nodes and render them.
	 * 
	 * Params:
	 * 		c = is a reference to the Cairo Context to render the Graph to.
	 */
	void renderCairo (ref Context c) {
		if (wall_) {
			c.save();
			c.setSourceRgb(0.8, 0.0, 0.0);
			c.setLineWidth(10.0);
			c.rectangle(x_, y_, width_, height_);
			c.stroke();
			c.restore();
		}
		foreach(ref e; edges_)
			e.renderCairo(c);
		foreach(ref n; nodes_)
			n.renderCairo(c);
	}
	
	void renderOpenGL () {
	}
	
	/**
	 * Exports the Graph to an XML tree. It is passed an XML-Node and creates 
	 * the XML description of the Graph as a child of it then exports the Nodes
	 * and Edges of the Graph to XML too.
	 * 
	 * Params:
	 * 		val = is the XML-Node to create the Graph XML description on.
	 */
	void toXML (ref Document!(char) val) {
		auto node = val
			.element(null, "Graph")	
			.attribute(null, "nodeNumber", Integer.toString(nodes_.length))
			.attribute(null, "edgeNumber", Integer.toString(edges_.length));
		
		foreach (ref n; nodes_) {
			n.toXML (node);
		}
		
		foreach (ref e; edges_) {
			e.toXML (node);
		}
	}
	
	void fromXML (char[] val) {
	}
	
	/* Getter and Setter Methods */
	
	bool wall () {
		return wall_;
	}
	
	void wall (bool val) {
		wall_ = val;
	}
	
	N[] nodes () {
		return nodes_;
	}
	
	E[] edges () {
		return edges_;
	}
	
	void x (int val) {
		x_ = val;
	}
	
	int x () {
		return x_;
	}
	
	void y (int val) {
		y_ = val;
	}
	
	int y () {
		return y_;
	}
	
	void width (int val) {
		width_ = val;
	}
	
	int width () {
		return width_;
	}
	
	void height (int val) {
		height_ = val;
	}
	
	int height () {
		return height_;
	}
}
