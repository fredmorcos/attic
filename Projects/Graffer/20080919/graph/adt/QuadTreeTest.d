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

module graph.adt.QuadTreeTest;

private import
	graph.adt.QuadTree,
	graph.adt.Quadrant,
	graph.adt.Vertex,
	tango.io.Stdout;

unittest {
	QuadTree* tree = new QuadTree;
	tree.quadrant.x = 0;
	tree.quadrant.y = 0;
	tree.quadrant.length = 30;
	Vertex* v0 = new Vertex,
			v1 = new Vertex,
			v2 = new Vertex,
			v3 = new Vertex,
			v4 = new Vertex;
			
	v0.pos.x = 5;
	v0.pos.y = 5;
	v1.pos.x = 20;
	v1.pos.y = 5;
	v2.pos.x = 5;
	v2.pos.y = 20;
	v3.pos.x = 10;
	v3.pos.y = 10;
	v4.pos.x = -15;
	v4.pos.y = -10;
	
	v0.size = 5;
	v1.size = 5;
	v2.size = 5;
	v3.size = 5;
	v4.size = 5;
	
	tree.insertVertex(v0);
	tree.insertVertex(v1);
	tree.insertVertex(v2);
	tree.insertVertex(v3);
//	tree.insertVertex(v4);
	
	tree.calculateCenterOfMass();
	tree.printInfo(0);
	
	delete v0;
	delete v1;
	delete v2;
	delete v3;
	delete v4;
	tree.destroy();
	delete tree;
	Stdout("Center of Mass test passed.").newline;
}

unittest {
	QuadTree* tree = new QuadTree;
	tree.quadrant.x = -20;
	tree.quadrant.y = -20;
	tree.quadrant.length = 30;
	Vertex* v0 = new Vertex,
			v1 = new Vertex,
			v2 = new Vertex,
			v3 = new Vertex,
			v4 = new Vertex;
			
	v0.pos.x = -15;
	v0.pos.y = -15;
	v1.pos.x = -10;
	v1.pos.y = -20;
	v2.pos.x = -20;
	v2.pos.y = -10;
	v3.pos.x = -10;
	v3.pos.y = -10;
	v4.pos.x = -15;
	v4.pos.y = -10;
	
	v0.size = 5;
	v1.size = 5;
	v2.size = 5;
	v3.size = 5;
	v4.size = 5;
	
	tree.insertVertex(v0);
	tree.insertVertex(v1);
	tree.insertVertex(v2);
	tree.insertVertex(v3);
	tree.insertVertex(v4);
	
	tree.calculateCenterOfMass();
	tree.printInfo(0);
	
	delete v0;
	delete v1;
	delete v2;
	delete v3;
	delete v4;
	tree.destroy();
	delete tree;
	Stdout("Barnes-Hut QuadTree test passed.").newline;
}

unittest {
	QuadTree* tree = new QuadTree;
	tree.quadrant.x = -20;
	tree.quadrant.y = -20;
	tree.quadrant.length = 30;
	assert(tree.getChildQuadrantByPosition(-15, -15) == 0);
	assert(tree.getChildQuadrantByPosition(-10, -20) == 0);
	assert(tree.getChildQuadrantByPosition(0, -10) == 1);
	assert(tree.getChildQuadrantByPosition(-10, 0) == 2);
	assert(tree.getChildQuadrantByPosition(5, 5) == 3);
	delete tree.child[0];
	tree.destroy();
	delete tree;
	Stdout("QuadTree test passed.").newline;
}
