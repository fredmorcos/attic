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

module graph.xml.GraphFile;

private import
	graph.adt.Graph, 
	graph.adt.Edge,
	graph.adt.Vertex,
	tango.io.File, 
	tango.text.xml.Document,
	tango.text.xml.DocPrinter, 
	tango.text.Ascii,
	tango.io.Stdout,
	Float = tango.text.convert.Float,
	Integer = tango.text.convert.Integer;

void saveGraph(Graph *g, char[] fileName) {
	auto	document = new Document!(char),
			printer = new DocPrinter!(char);
	File	file = new File(fileName);
			
	document.header();
	auto graphNode = document.root	.element(null, "Graph")
									.attribute(null, "VertexNumber", 
										Integer.toString(g.vertices.length))
									.attribute(null, "EdgeNumber", 
										Integer.toString(g.edges.length));
	
	foreach(i, ref v; g.vertices) {
		graphNode	.element(null, "Vertex")
					.attribute(null, "id", Integer.toString(i))
					.attribute(null, "x", Float.toString(v.pos.x))
					.attribute(null, "y", Float.toString(v.pos.y));
	}
	
	foreach(ref e; g.edges) {
		graphNode	.element(null, "Edge")
					.attribute(null, "sourceid", 
							Integer.toString(g.getVertexIndex(e.v1)))
					.attribute(null, "targetid", 
							Integer.toString(g.getVertexIndex(e.v2)));
	}
	
	file.write(printer(document));
	
	delete file;
	delete graphNode;
	delete printer;
	delete document;
}

Graph *loadGraph(char[] fileName) {
	Graph	*g = new Graph();
	auto	document = new Document!(char);
	File	file = new File(fileName);
	char[]	fileData = cast(char[]) file.read();
	int		id, 
			sourceID, 
			targetID,
			i;
	double	x,
			y;
	
	document.parse(fileData);
	
	foreach(node; document.query.child("Graph")) {
		foreach(attr; node.attributes()) {
			if (compare(attr.name, "VertexNumber") == 0)
				g.vertices.length = Integer.parse(attr.value());
			else if (compare(attr.name, "EdgeNumber") == 0)
				g.edges.length = Integer.parse(attr.value());
			else
				Stdout("DEBUG (file.GraphFile.d) loadGraph()").newline;
		}
		
		i = 0;
		foreach(child; node.children()) {
			if (compare(child.name(), "Vertex") == 0) {
				foreach(attr; child.attributes()) {
					if (compare(attr.name, "id") == 0)
						id = Integer.parse(attr.value());
					else if (compare(attr.name, "x") == 0)
						x = Integer.parse(attr.value());
					else if (compare(attr.name, "y") == 0)
						y = Integer.parse(attr.value());
				}
				g.vertices[id] = new Vertex();
				g.vertices[id].pos.x = x;
				g.vertices[id].pos.y = y;
				g.vertices[id].size = 40;
				g.vertices[id].drawSize = g.vertices[id].size / 6;
			}
			
			if (compare(child.name(), "Edge") == 0) {
				foreach(attr; child.attributes()) {
					if (compare(attr.name, "sourceid") == 0)
						sourceID = Integer.parse(attr.value());
					else if (compare(attr.name, "targetid") == 0)
						targetID = Integer.parse(attr.value());
				}
				g.edges[i] = new Edge();
				g.edges[i].v1 = g.vertices[sourceID];
				g.edges[i].v2 = g.vertices[targetID];
				++i;
			}
		}
	}
	
	delete document;
	delete file;
	
	return g;
}
