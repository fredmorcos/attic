/*
	This file is part of Grafer.
	
	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	Grafer is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License version 3 
	as published by	the Free Software Foundation.

	Grafer is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Grafer. If not, see <http://www.gnu.org/licenses/>.
*/

#import "layout-simple.h"
#import <math.h>

@implementation LayoutSimple

+ layout: (LayoutOps *) ops {
	int		i = 0,										/* curNodes counter */
			j = 0;										/* othNodes counter */
	Node	*curNode, 									/* current node */
			*othNode;									/* other node */
	Edge	*curEdge;									/* current edge */

	ops->energy->x = 0.0;
	ops->energy->y = 0.0;

	i = 0;
	while (i < ops->nodesNum) {
		curNode = [ops->nodeList getItem: i];
		
		if ([curNode lock]) {
			i++;
			continue;
		}

		j = 0;
		while (YES) {
			if (j >= ops->nodesNum && j >= ops->edgesNum)
				break;

			/* repulsion */
			if (j < ops->nodesNum) {
				othNode = [ops->nodeList getItem: j];
				if (curNode != othNode)
					[self repulsion: ops: curNode: othNode];
			}

			/* attraction */
			if (j < ops->edgesNum) {
				curEdge = [ops->edgeList getItem: j];
				if (curNode == [curEdge pointA] || curNode == [curEdge pointB])
					[self attraction: ops: curNode: curEdge];
			}

			j++;
		}

		/* update node information */
		[Layout updateNode: ops: curNode];

		i++;
	}

	return self;
}

/* hooke attraction vector components for x and y directions */
+ attraction: (LayoutOps *) ops: (Node *) node: (Edge *) edge {
	Node	*nodeA,
			*nodeB;

	if (node == [edge pointA]) {
		nodeA = (Node *)[edge pointA];
		nodeB = (Node *)[edge pointB];
	}
	else {
		nodeB = (Node *)[edge pointA];
		nodeA = (Node *)[edge pointB];
	}

	Point	*nodeANetforce = [nodeA netforce];
	double	dx = nodeA->x - nodeB->x,
			dy = nodeA->y - nodeB->y,
//			denom = sqrt(dx * dx + dy * dy),
			resx = -ops->attConst * dx, // / denom,
			resy = -ops->attConst * dy; // / denom;

	nodeANetforce->x += resx;
	nodeANetforce->y += resy;

	return self;
}

/* coulomb repulsion vector components in x and y directions */
+ repulsion: (LayoutOps *) ops: (Node *) node1: (Node *) node2 {
	double	masses = [node1 size] * [node2 size],
			dx = node1->x - node2->x,
			dy = node1->y - node2->y,
			denom = pow(dx * dx + dy * dy, (3 / 2)),
			numer = masses * ops->repConst,
			resx = numer * dx / denom,
			resy = numer * dy / denom;
	Point	*node1Netforce = [node1 netforce];

	node1Netforce->x += resx;
	node1Netforce->y += resy;

	return self;
}

@end

