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
	graph.adt.interfaces.Exportable,
	graph.adt.interfaces.GraphEdge,
	graph.adt.interfaces.Renderable,
	tango.text.xml.Document,
	cairo.Context;

/**
 * An Edge, Arc or Connection in a Graph represented by a straight line which 
 * represents a pair of Nodes, connecting them together. 
 * 
 * Implements the GraphEdge and Renderable Interfaces.
 * The Source and Target have to at least implement the Boundary Interface.
 */
class Edge(N):
	GraphEdge!(N),
	Renderable,
	Exportable {
	
	private:
	
	N	source_,	/** Source Node of the Edge. */
		target_;	/** Target Node of the Edge. */
		
	public:
	
	/**
	 * Renders the Edge to a Cairo Context. Moves to the position of the Source, 
	 * draws a straight line to the position of Target and flushes the changes 
	 * to a Cairo Context.
	 * 
	 * Params:
	 * 		c = is a reference of the Cairo Context to render the Edge to.
	*/
	void renderCairo (ref Context c) {
		c.save ();
		c.moveTo (source_.getCenterX, source_.getCenterY);
		c.lineTo (target_.getCenterX, target_.getCenterY);
		c.stroke ();
	}
	
	void renderOpenGL () {
	}
	
	/**
	 * Exports the Edge to an XML tree. It is passed an XML-Node and creates 
	 * the XML description of the Edge as a child of it.
	 * 
	 * Params:
	 * 		val = is the XML-Node to create the Edge XML description on.
	 */
	void toXML (ref Document!(char) val) {
		val	
			.element (null, "Edge")
			.attribute (null, "sourceId", Integer.toString (source_.id))
			.attribute (null, "targetId", Integer.toString (target_.id));
	}
	
	void fromXML (char[] val) {
	}
	
	/* Getter and Setter Methods */
	
	void source (N val) {
		source_ = val;
	}
	
	N source () {
		return source_;
	}
	
	void target (N val) {
		target_ = val;
	}
	
	N target () {
		return target_;
	}
}
