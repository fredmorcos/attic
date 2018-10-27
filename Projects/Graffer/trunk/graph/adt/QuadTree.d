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

module graph.adt.QuadTree;

private import
	graph.adt.Quadrant,
	graph.adt.Vertex,
	tango.io.Stdout,
	tango.math.Math;

/**
	\brief	A quadtree node.
	
	A quadtree node containing the space Quadrant it represents, a 
	Vertex representing the mass and center of mass of the vertices in 
	the Quadrant and four children.
 */
struct QuadTree {
	Quadrant		quadrant;	/**< The occupied space Quadrant. */
	Vertex*			vertex;		/**< The mass and center of mass. */
	QuadTree*[4]	child;		/**< The children. */

	/**
	 * \brief Recursive Vertex insertion into a QuadTree.
	 */
	void insertVertex(Vertex* v) {
		int childNum;
		
		if (!vertex)
			vertex = v;
		else {
			if (isLeaf()) {
				/* temporary trick */
				if (v.pos.x == vertex.pos.x && v.pos.y == vertex.pos.y) {
					v.pos.x += min(20, quadrant.length - v.pos.x);
					v.pos.y += min(20, quadrant.length - v.pos.y);
				}
				
				childNum = getChildQuadrantByPosition(v.pos.x, v.pos.y);
				createChildQuadTree(childNum);
				child[childNum].insertVertex(v);
				childNum = getChildQuadrantByPosition(vertex.pos.x, vertex.pos.y);
				createChildQuadTree(childNum);
				child[childNum].insertVertex(vertex);
			}
			else {
				childNum = getChildQuadrantByPosition(v.pos.x, v.pos.y);
				createChildQuadTree(childNum);
				child[childNum].insertVertex(v);
			}
		}
	}
	
	/**
	 * \brief Creates the appropriate child with index.
	 */
	void createChildQuadTree(int index) {
		if (!child[index]) {
			child[index] = new QuadTree;
			child[index].quadrant.length = quadrant.length / 2;
			switch(index) {
				case 0:
					child[index].quadrant.x = quadrant.x;
					child[index].quadrant.y = quadrant.y;
					break;
				case 1:
					child[index].quadrant.x = quadrant.x + 
									child[index].quadrant.length;
					child[index].quadrant.y = quadrant.y;
					break;
				case 2:
					child[index].quadrant.x = quadrant.x;
					child[index].quadrant.y = quadrant.y + 
									child[index].quadrant.length;
					break;
				case 3:
					child[index].quadrant.x = quadrant.x + 
									child[index].quadrant.length;
					child[index].quadrant.y = quadrant.y + 
									child[index].quadrant.length;
					break;
			}
		}
	}
	
	/**
	 * \brief Recursively calculates the center of mass of each node under a 
	 * QuadTree.
	 */
	Vertex* calculateCenterOfMass() {
		if (isLeaf())
			return vertex;
		
		Vertex*	tempVertex;
		vertex = new Vertex;
		
		foreach(ref c; child) {
			if (c) {
				tempVertex = c.calculateCenterOfMass();
				vertex.size += tempVertex.size;
				vertex.pos.x += (tempVertex.pos.x * tempVertex.size);
				vertex.pos.y += (tempVertex.pos.y * tempVertex.size);
			}
		}
		
		vertex.pos.x /= vertex.size;
		vertex.pos.y /= vertex.size;
		
		return vertex;
	}
	
	/*
	 * ________
	 * |0  |1  |
	 * |__ |__ |
	 * |2  |3  |
	 * |___|___|
	 */
	
	/**
	 * \brief Returns the index of the quadrant of a vertex positioned at x, y.
	 * 
	 * \return Index of quadrant where position x, y belongs.
	 */
	int getChildQuadrantByPosition(double x, double y) {
		if (x >= quadrant.x && x <= quadrant.getCenterX())		/* 0 or 2 */
			if (y >= quadrant.y && y <= quadrant.getCenterY())	/* 0 */
				return 0;
			else 												/* 2 */
				return 2;
		else 													/* 1 or 3 */
			if (y >= quadrant.y && y <= quadrant.getCenterY())	/* 1 */
				return 1;
			else 												/* 3 */
				return 3;
	}
	
	/**
		\brief	Returns true if the QuadTree is a leaf.
		
		\return True if the QuadTree is a leaf, false otherwise.
	 */
	bool isLeaf() {
		foreach(c; child)
			if(c) return false;
		return true;
	}
	
	/**
	 * \brief Prints the tree. Used for debugging.
	 */
	void printInfo(int tabs) {
		for(int i = 0; i < tabs; i++)
			Stdout.format("\t");
		Stdout.formatln("Node - Mass: {}, X: {}, Y: {}.", 
				vertex.size, vertex.pos.x, vertex.pos.y);
		foreach(i, c; child)
			if (c) {
				Stdout.format("{} ", i);
				c.printInfo(tabs + 1);
			}
	}
	
	/**
	 * \brief Destroys and frees the tree.
	 */
	void destroy() {
		if (!isLeaf())
			delete vertex;
		
		foreach(c; child)
			if (c) {
				c.destroy();
				delete c;
			}
	}
}
