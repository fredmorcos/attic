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

module graph.alg.Shape;

private import
	graph.adt.Graph, 
	graph.adt.Vertex, 
	graph.adt.Edge, 
	graph.adt.Vector, 
	tango.math.random.Kiss;

/**
	\brief	Creates the Graph's list of edges depending on the GraphShape 
	template.
	
	\param	g is a pointer to the Graph to be shaped.
	\param	shape is the GraphShape type template of the Graph.
 */
public void createEdges (Graph *g, GraphShape shape) {
	switch (shape) {
		case GraphShape.CIRCULAR:
			g.edges.length = g.vertices.length;
			circularEdges(g);
			break;
		case GraphShape.CENTERED:
			g.edges.length = g.vertices.length - 1;
			centeredEdges(g);
			break;
		case GraphShape.INTERCONNECTED:
			g.edges.length = g.vertices.length * 
					(g.vertices.length - 1) / 2;
			interconnectedEdges(g);
			break;
		case GraphShape.BINARYTREE:
			g.edges.length = g.vertices.length - 1;
			binarytreeEdges(g);
			break;
		default:	/* no edges */
			g.edges.length = 0;
	}
}

/**
	\brief	Create and randomly position the list of vertices of a Graph.
	
	Create and randomly position each Vertex in the list of vertices in Graph on 
	the frame without bypassing the boundaries of the Graph frame.
	
	\param	g is a pointer to the Graph containing the list of vertices to be 
	randomly positioned.
	\param	size is the number of vertices to be created and positioned.
 */
public void randomVertices (Graph *g, int size) {
	g.vertices.length = size;
	foreach(ref v; g.vertices) {
		v = new Vertex;
		v.size = 40;
		v.drawSize = v.size / 6;
		v.pos.x = Kiss.instance.toInt(v.size, g.width - v.size);
		v.pos.y = Kiss.instance.toInt(v.size, g.height - v.size);
	}
}

/**
	\brief	Create a circular Graph.
	
	\param	g is a pointer to the Graph to be created.
 */
private void circularEdges (Graph *g) {
	foreach(i, ref v; g.vertices) {
		g.edges[i] = new Edge;
		g.edges[i].v1 = v;
		if (i == g.vertices.length - 1)
			g.edges[i].v2 = g.vertices[0];
		else
			g.edges[i].v2 = g.vertices[i + 1];
	}
}

/**
	\brief	Create a centered Graph.
	
	\param	g is a pointer to the Graph to be created.
 */
private void centeredEdges (Graph *g) {
	foreach(i, ref v; g.vertices[1 .. $]) {
		g.edges[i] = new Edge;
		g.edges[i].v1 = g.vertices[0];
		g.edges[i].v2 = v;
	}
}

/**
	\brief	Create an interconnected Graph.
	
	\param	g is a pointer to the Graph to be created.
 */
private void interconnectedEdges (Graph *g) {
	int			edgeIndex = 0;

	foreach(i, ref v; g.vertices) {
		foreach(ref w; g.vertices[i + 1 .. $]) {
			g.edges[edgeIndex] = new Edge;
			g.edges[edgeIndex].v1 = v;
			g.edges[edgeIndex].v2 = w;
			++edgeIndex;
		}
	}
}

/**
	\brief	Create a binary tree Graph.
	
	\param	g is a pointer to the Graph to be created.
 */
private void binarytreeEdges (Graph *g) {
	int			othVertexIndex = 0,
				edgeIndex = 0;

	foreach(i, ref v; g.vertices) {
		othVertexIndex = i * 2;

		if (othVertexIndex + 1 > g.vertices.length - 1)
			break;
		else {
			g.edges[edgeIndex] = new Edge;
			g.edges[edgeIndex].v1 = g.vertices[i];
			g.edges[edgeIndex].v2 = g.vertices[othVertexIndex + 1];
			edgeIndex++;
		}

		if (othVertexIndex + 2 > g.vertices.length - 1)
			break;
		else {
			g.edges[edgeIndex] = new Edge;
			g.edges[edgeIndex].v1 = g.vertices[i];
			g.edges[edgeIndex].v2 = g.vertices[othVertexIndex + 2];
			edgeIndex++;
		}
	}
}

