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

module graph.alg.TimeForceLayout;

private import
	graph.alg.SimpleForceLayout,
	graph.adt.Edge,
	graph.adt.Graph,
	graph.adt.Vector,
	graph.adt.Vertex,
	tango.math.Math,
	tango.io.Stdout;

/**
	\brief	Implementation of a time improved force directed layouting 
	algorithm.
 */
class TimeForceLayout: SimpleForceLayout {
public:
	/** Constructor. */
	this() {
		super();
	}

	/** Print algorithm information. */
	void printInfo() {
		Stdout("Time Force Layout").newline;
		Stdout.formatln("Number of vertices: {}", graph.vertices.length);
		Stdout.formatln("Number of edges: {}", graph.edges.length);
		Stdout.formatln("Electric constant: {}", eConst);
		Stdout.formatln("Spring constant: {}", sConst);
		Stdout.formatln("Damping value: {}", damping);
		Stdout.formatln("Timestep value: {}", timestep);	
	}

	/**
		Algorithm's starting point. Iterates over all vertices calculating 
		repulsive forces. Then, iterates over all edges calculating attractive
		forces. Vertex positions are then updated simultaneously.
	 */
	void run() {
		energy.x = 0.0;
		energy.y = 0.0;

		foreach(i, ref curV; graph.vertices)
			foreach(ref othV; graph.vertices[i + 1 .. $])
				repulsion(curV, othV);

		foreach(ref curE; graph.edges)
			if (sNL)
				attractionNL(curE);
			else
				attraction(curE);

		foreach(ref curV; graph.vertices)
			update(curV);
	}

protected:
	/**
	  	Spring attraction method, following Hooke's Law for springs
		with natural length.
	 */
	void attractionNL(Edge* e) {
		Vector	delta,
				res;
		double	distance,
				factor;
		Vertex*	a = e.v1,
				b = e.v2;
		
		delta.x = a.pos.x - b.pos.x;
		delta.y = a.pos.y - b.pos.y;

		version(optimize) {
			distance = __sse_sqrt(delta.x * delta.x + delta.y * delta.y);
		}
		else {
			distance = sqrt(delta.x * delta.x + delta.y * delta.y);
		}
		
		factor = -sConst * (distance - sLength) / distance;

		res.x = delta.x * factor;
		res.y = delta.y * factor;

		a.netf.x += res.x;
		a.netf.y += res.y;
		b.netf.x -= res.x;
		b.netf.y -= res.y;
	}

	/**
		Spring attraction method, following Hooke's Law
	 */
	void attraction(Edge* e) {
		Vertex*	a = e.v1,
				b = e.v2;
		Vector	res;

		res.x = -sConst * (a.pos.x - b.pos.x);
		res.y = -sConst * (a.pos.y - b.pos.y);

		a.netf.x += res.x;
		a.netf.y += res.y;
		b.netf.x -= res.x;
		b.netf.y -= res.y;
	}

	/**
		Charge repulsion method, following Coulomb's Law
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
		v2.netf.x -= res.x;
		v2.netf.y -= res.y;
	}
}
