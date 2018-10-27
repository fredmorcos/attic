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

#import "layout.h"
#import "layout-fr.h"
#import "layout-simple.h"
#import <graph.h>
#import <math.h>

@implementation Layout

+ forceLayout: (LayoutOps *) ops {
	switch(ops->type) {
		case ALG_TYPE_SIMPLE:
			[LayoutSimple layout: ops];
			break;
		case ALG_TYPE_FR:
			[LayoutFR layout: ops];
			break;
		case ALG_TYPE_EADES:
			break;
		case ALG_TYPE_GEM:
			break;
		case ALG_TYPE_GRID:
			break;
		case ALG_TYPE_BH:
			break;
	}
	
	return self;
}

+ updateNode: (LayoutOps *) ops: (Node *) node {
	Point	*velocity = [node velocity],				/* node velocity */
			*netforce = [node netforce];				/* node netforce */
	int		nodeSize = [node size];						/* size of node */

	/* node velocity */
	velocity->x = (velocity->x + netforce->x) * ops->damping;
	velocity->y = (velocity->y + netforce->y) * ops->damping;

	node->x += velocity->x * ops->damping;
	node->y += velocity->y * ops->damping;

	if (ops->useWall) {
		if (node->x < nodeSize) {
			node->x = nodeSize;
			velocity->x = 0.0;
		}
		if (node->y < nodeSize) {
			node->y = nodeSize;
			velocity->y = 0.0;
		}
		if (node->x > ops->frameWidth - nodeSize) {
			node->x = ops->frameWidth - nodeSize;
			velocity->x = 0.0;
		}
		if (node->y > ops->frameHeight - nodeSize) {
			node->y = ops->frameHeight - nodeSize;
			velocity->y = 0.0;
		}
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

