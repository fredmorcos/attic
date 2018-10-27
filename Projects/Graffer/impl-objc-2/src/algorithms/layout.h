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

#import <objc/Object.h>
#import <graph.h>

typedef enum _AlgorithmType {
	ALG_TYPE_SIMPLE,
	ALG_TYPE_FR,
	ALG_TYPE_EADES,
	ALG_TYPE_GEM,
	ALG_TYPE_GRID,
	ALG_TYPE_BH
} AlgorithmType;

typedef struct _LayoutOps {
	AlgorithmType	type;
	int				nodesNum,
					edgesNum,
					frameWidth,
					frameHeight;
	double			repConst,
					attConst,
					damping;
	BOOL			useWall;
	List			*nodeList,
					*edgeList;
	Point			*energy;
} LayoutOps;

@interface Layout: Object {
}

+ forceLayout: (LayoutOps *) ops;
+ updateNode: (LayoutOps *) ops: (Node *) node;

@end

