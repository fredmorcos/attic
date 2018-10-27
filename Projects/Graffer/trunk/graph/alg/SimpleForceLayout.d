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

module graph.alg.SimpleForceLayout;

private import
	graph.alg.ForceLayout, 
	graph.adt.Edge, 
	graph.adt.Graph, 
	graph.adt.Vector, 
	graph.adt.Vertex, 
	tango.math.Math, 
	tango.io.Stdout;

/**
	\brief	Implementation of a simple force directed layouting algorithm.
 */
class SimpleForceLayout: ForceLayout {
public:
	/** Constructor. */
	this() {
		super();
	}

	/** Print algorithm information. */
	void printInfo() {
		Stdout("Simple Force Layout").newline;
		Stdout.formatln("Number of vertices: {}", graph.vertices.length);
		Stdout.formatln("Number of edges: {}", graph.edges.length);
		Stdout.formatln("Electric constant: {}", eConst);
		Stdout.formatln("Spring constant: {}", sConst);
		Stdout.formatln("Damping value: {}", damping);
		Stdout.formatln("Timestep value: {}", timestep);
	}

	/**
		Algorithm's starting point. Iterates over all vertices calculating 
		repulsive and attractive forces. Vertex positions are updated 
		independently.
	 */
	void run() {
		energy.x = 0.0;
		energy.y = 0.0;

		foreach(ref curV; graph.vertices) {
			if(curV.locked)
				continue;

			foreach(ref othV; graph.vertices)
				if (curV != othV)
					repulsion(curV, othV);

			foreach(ref curE; graph.edges)
				if (curV == curE.v1 || curV == curE.v2)
					if (sNL)
						attractionNL(curE, curV);
					else
						attraction(curE, curV);

			update(curV);
		}
	}

protected:
	/**
	  	Spring attraction method, following Hooke's Law for springs
		with natural length.
	 */
	void attractionNL(Edge* e, Vertex* v) {
		Vector	delta;
		double	distance,
				factor;
		Vertex*	a,
				b;

		if (v == e.v1) {
			a = e.v1;
			b = e.v2;
		}
		else {
			b = e.v1;
			a = e.v2;
		}
		
		delta.x = a.pos.x - b.pos.x;
		delta.y = a.pos.y - b.pos.y;
		
		version(optimize) {
			distance = __sse_sqrt(delta.x * delta.x + delta.y * delta.y);
		}
		else {
			distance = sqrt(delta.x * delta.x + delta.y * delta.y);
		}
		
		factor = -sConst * (distance - sLength) / distance;

		a.netf.x += delta.x * factor;
		a.netf.y += delta.y * factor;
	}

	/**
		Spring attraction method, following Hooke's Law
	 */
	void attraction(Edge* e, Vertex* v) {
		Vertex*	a,
				b;
		Vector	dis;
//		double	denom;

		if (v == e.v1) {
			a = e.v1;
			b = e.v2;
		}
		else {
			b = e.v1;
			a = e.v2;
		}

		dis.x = a.pos.x - b.pos.x;
		dis.y = a.pos.y - b.pos.y;
//		denom = sqrt(dis.x * dis.x + dis.y * dis.y);
		a.netf.x += -sConst * dis.x; // / denom;
		a.netf.y += -sConst * dis.y; // / denom;
	}

	/**
		Charge repulsion method, following Coulomb's Law
	 */
	void repulsion(Vertex* v1, Vertex* v2) {
		Vector	dis;
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

		v1.netf.x += numer * dis.x / denom;
		v1.netf.y += numer * dis.y / denom;
	}

	/**
		Vertex position update and system energy calculation method
	 */
	void update(Vertex* v) {
		v.vel.x = (v.vel.x + timestep * v.netf.x) * damping;
		v.vel.y = (v.vel.y + timestep * v.netf.y) * damping;
		v.pos.x += v.vel.x * timestep;
		v.pos.y += v.vel.y * timestep;

		if (graph.wall) {
			if(v.pos.x < v.drawSize) {
				v.pos.x = v.drawSize;
				v.vel.x = 0.0;
			}
			if(v.pos.x > graph.width - v.drawSize) {
				v.pos.x = graph.width - v.drawSize;
				v.vel.x = 0.0;
			}
			if(v.pos.y < v.drawSize) {
				v.pos.y = v.drawSize;
				v.vel.y = 0.0;
			}
			if(v.pos.y > graph.height - v.drawSize) {
				v.pos.y = graph.height - v.drawSize;
				v.vel.y = 0.0;
			}
		}

		energy.x += v.size * v.vel.x * v.vel.x / 2;
		energy.y += v.size * v.vel.y * v.vel.y / 2;

		v.netf.x = 0.0;
		v.netf.y = 0.0;
	}
}
