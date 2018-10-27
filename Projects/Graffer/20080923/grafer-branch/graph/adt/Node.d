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

module graph.adt.Node;

private import
	graph.adt.interfaces.GraphNode,
	graph.adt.interfaces.LayoutableNode,
	graph.adt.interfaces.Exportable,
	graph.adt.Vector,
	cairo.Context,
	tango.math.Math,
	tango.text.xml.Document,
	Integer = tango.text.convert.Integer;

/**
 * Represents a Vertex or Node in a Graph. It can hold a single item (Object or 
 * Type) which has to at least implement the Exportable and Renderable 
 * interfaces.
 * 
 * Implements the LayoutableNode (and Boundary) Interface.
 */
class Node(T):
	GraphNode!(T),
	LayoutableNode, 
	Exportable {
	
	private:
	
	T		data_;		/** The item that the Node is holding/ */
	int		x_,			/** X position of the Node. See Boundary Interface. */
			y_,			/** Y position of the Node. See Boundary Interface. */
			width_,		/** Width of the Node. See Boundary Interface. */
			height_,	/** Height of the Node. See Boundary Interface. */
			id_;
	bool	locked_;	/** Whether the Node is locked or not. */
	Vector	velocity_,	/** Velocity of the Node when Layouting. */
			netforce_;	/** Netforce on the Node when Layouting. */
	
	public:
	
	/**
	 * Renders the Node to a Cairo Context. Saves the current state of the Cairo 
	 * Context's properties. If the Node is locked, sets the color to red. Then, 
	 * renders a circular arc and fills it - creating a circle - and restores 
	 * the Cairo Context's settings.
	 * 
	 * Params:
	 * 		c = is a reference to the Cairo Context to render the Node to.
	 */
	void renderCairo (ref Context c) {
		c.save ();

		c.moveTo(x_, y_);
		data_.renderCairo (c, width_, height_);

		if (locked) {
			c.setSourceRgb (0.8, 0.0, 0.0);
			c.arc (x_, y_, 5, 0, 2 * PI);
			c.closePath ();
			c.fill ();
		}
		
		c.restore ();
	}
	
	void renderOpenGL () {
	}
	
	/**
	 * Exports the Node to an XML-Node. It is passed an XML-Node and creates 
	 * the XML description of the Node as a child of it then exports its data 
	 * item to XML too.
	 * 
	 * Params:
	 * 		val = is the XML-Node to create the Node XML description on.
	 */
	void toXML (ref Document!(char) val) {
		auto node = val
			.element(null, "Vertex")
			.attribute(null, "id", Integer.toString(id_))
			.attribute(null, "x", Integer.toString(x_))
			.attribute(null, "y", Integer.toString(y_))
			.attribute(null, "width", Integer.toString(width_))
			.attribute(null, "height", Integer.toString(height_))
			.attribute(null, "locked", Integer.toString(locked_));
		data_.toXML (node);
	}
	
	void fromXML (char[] val) {
	}
	
	/* Getter and Setter Methods */
	
	int getCenterX () {
		return x_ + (width_ / 2);
	}
	
	int getCenterY () {
		return y_ + (height_ / 2);
	}
	
	void data (T val) {
		data_ = val;
	}
	
	T data () {
		return data_;
	}
	
	void id (int val) {
		id_ = val;
	}
	
	int id () {
		return id_;
	}
	
	void velocity (Vector val) {
		velocity_ = val;
	}
	
	Vector velocity () {
		return velocity_;
	}
	
	void netforce (Vector val) {
		netforce_ = val;
	}
	
	Vector netforce () {
		return netforce_;
	}
	
	void locked (bool val) {
		locked_ = val;
	}
	
	bool locked () {
		return locked_;
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
