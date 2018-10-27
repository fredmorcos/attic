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

module graph.alg.BHForceLayout;

private import
	graph.alg.TimeForceLayout,
	graph.adt.Vertex,
	graph.adt.Vector,
	graph.adt.Edge,
	graph.adt.Graph,
	graph.adt.QuadTree,
	graph.adt.Quadrant,
	tango.io.Stdout,
	tango.math.Math;

class BHForceLayout: TimeForceLayout {
public:
	double	theta = 0.9;		/**< Barnes-Hut theta. */

	/** Constructor. */
	this() {
		super();
	}
	
	/** Print algorithm information. */
	void printInfo() {
		Stdout("BH Force Layout").newline;
		Stdout.formatln("Number of vertices: {}", graph.vertices.length);
		Stdout.formatln("Number of edges: {}", graph.edges.length);
		Stdout.formatln("Electric constant: {}", eConst);
		Stdout.formatln("Spring constant: {}", sConst);
		Stdout.formatln("Damping value: {}", damping);
		Stdout.formatln("Timestep value: {}", timestep);	
	}
	
	/**
		Algorithm's starting point. Creates the Barnes-Hut QuadTree, iterates 
		over all vertices calculating repulsive forces. Then, iterates over all 
		edges calculating attractive forces. Vertex positions are then updated 
		simultaneously.
	 */
	void run() {
		QuadTree*	tree = new QuadTree;		/**< Root of the tree. */
		
		graph.getQuadrant(tree.quadrant);
		
		foreach(i, ref v; graph.vertices)
			tree.insertVertex(v);
		
		tree.calculateCenterOfMass();
		
		energy.x = 0.0;
		energy.y = 0.0;
		
		foreach(ref v; graph.vertices)
			repulsion(v, tree);
		
		foreach(ref curE; graph.edges)
			if (sNL)
				attractionNL(curE);
			else
				attraction(curE);
		
		foreach(ref curV; graph.vertices)
			update(curV);
		
		tree.destroy();
		delete tree;
	}
	
protected:
	/**
		Barnes-Hut recursive force calculation.
	 */
	void repulsion(Vertex* v, QuadTree* t) {
		if (t.vertex == v)
			return;
		else {
			if (t.isLeaf())
				repulsion(v, t.vertex);
			else {
				double ratio = t.quadrant.length / distance(v, t.vertex);
				if (ratio < theta)
					repulsion(v, t.vertex);
				else
					foreach(c; t.child)
						if(c)
							repulsion(v, c);
			}
		}
	}

	/**
		Charge repulsion method, following Coulomb's Law.
	 */
	void repulsion(Vertex* v1, Vertex* v2) {
		Vector	dis,
				res;
		double	numer,
				denom;

		dis.x = v1.pos.x - v2.pos.x;
		dis.y = v1.pos.y - v2.pos.y;

		numer = v1.size * v2.size * eConst;
		// denom = pow(dis.x * dis.x + dis.y * dis.y, cast(real) 3 / 2);
		version(optimize) {
			denom = pow(__sse_sqrt(dis.x * dis.x + dis.y * dis.y), 3.0);
		}
		else {
			denom = pow(dis.x * dis.x + dis.y * dis.y, 1.5);
		}

		res.x = numer * dis.x / denom;
		res.y = numer * dis.y / denom;

		v1.netf.x += res.x;
		v1.netf.y += res.y;
	}
	
	/**
		Distance between two vertices.
	 */
	double distance(Vertex* v1, Vertex* v2) {
		double	dx = v1.pos.x - v2.pos.x,
				dy = v1.pos.y - v2.pos.y;
		
		version(optimize) {
			return __sse_sqrt(dx * dx + dy * dy);
		}
		else {
			return sqrt(dx * dx + dy * dy);
		}
	}
}
