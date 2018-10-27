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
		
//		if ([curNode lock]) {
//			i++;
//			continue;
//		}

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
		[self updateNode: ops: curNode];

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

+ updateNode: (LayoutOps *) ops: (Node *) node {
	Point	*velocity = [node velocity],				/* node velocity */
			*netforce = [node netforce];				/* node netforce */
	int		nodeSize = [node size];						/* size of node */

	/* node velocity */
//	velocity->x = (velocity->x + ops->timestep * netforce->x) * ops->damping;
//	velocity->y = (velocity->y + ops->timestep * netforce->y) * ops->damping;
	velocity->x = (velocity->x + netforce->x) * (ops->damping / 10.0);
	velocity->y = (velocity->y + netforce->y) * (ops->damping / 10.0);

	/* node position */
//	node->x += ops->timestep * velocity->x;
//	node->y += ops->timestep * velocity->y;

	node->x += velocity->x * ops->damping / 10.0;
	node->y += velocity->y * ops->damping / 10.0;

//	node->x += MIN(ABS(velocity->x), ABS((double)ops->frameWidth * ops->frameHeight / ops->nodesNum)) * (velocity->x / ABS(velocity->x));
//	node->y += MIN(ABS(velocity->y), ABS((double)ops->frameWidth * ops->frameHeight / ops->nodesNum)) * (velocity->y / ABS(velocity->y));

	if (ops->useWall) {
		if (node->x < nodeSize) 
			node->x = nodeSize;
		if (node->y < nodeSize) 
			node->y = nodeSize;
		if (node->x > ops->frameWidth - nodeSize) 
			node->x = ops->frameWidth - nodeSize;
		if (node->y > ops->frameHeight - nodeSize) 
			node->y = ops->frameHeight - nodeSize;
	}

	/* update energy of the system */
	ops->energy->x += nodeSize * velocity->x * velocity->x / 2;
	ops->energy->y += nodeSize * velocity->y * velocity->y / 2;

	/* reset netforce */
	netforce->x = 0.0;
	netforce->y = 0.0;

	return self;
}

@end

