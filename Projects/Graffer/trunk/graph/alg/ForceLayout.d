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

module graph.alg.ForceLayout;

private import
	graph.adt.Vertex, 
	graph.adt.Edge, 
	graph.adt.Graph, 
	graph.adt.Vector;

/**
	\brief	Enumeration for the layouting algorithms types.
	
	Represents the type of layouting algorithm to be used.
 */
public enum ForceLayoutType {
	SIMPLE,
	TIME,
	BH
}

/**
	\brief	Force directed layouting template.
	
	An abstract class representing a template for force-based layouting 
	algorithms to inherit from.
 */
abstract class ForceLayout: Object {
public:
	int		eConst,		/**< Electric constant. */
			sConst,		/**< Spring constant. */
			sLength,	/**< Spring natural length */
			nThreads;	/**< Number of threads to use */
	bool	sNL,		/**< Enable/Disable spring natural length */
			MT;			/**< Enable/Disable multi-threading */
	double	damping,	/**< Damping value. 0.1 <= damping >= 1.0. */
			timestep;	/**< Timestep value. */
	Vector	energy;		/**< Total energy in the Graph being layouted. */
	Graph	*graph;		/**< A pointer to the Graph to be layouted. */

	/** Constructor. */
	this() {
	}

	/** Starting point for the algorithm. */
	abstract void run();

	/** Information about algorithm. */
	abstract void printInfo();

protected:
	/** Vertex position update method. */
	abstract void update(Vertex* v);
	
	/* 
	 * http://www.oroboro.com/rafael/docserv.php/index/programming/article/distance
	 * 
	 * SSE square root
	 */
version(optimize) {
	double __sse_sqrt (double num) {
		float	res,
				n = cast(float) num;
		asm {
			movss		XMM0,	n		;
			rsqrtss 	XMM1,	XMM0	;
			mulps   	XMM1,	XMM0	;
			movss   	res,	XMM1	;
		}
		return cast(double) res;
	}
}
}

