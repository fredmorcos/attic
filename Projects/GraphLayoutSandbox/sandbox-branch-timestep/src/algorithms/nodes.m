#import "nodes.h"
#import <graph.h>
#import <node.h>

@implementation Nodes

+ nodeRandom: (Node *) node: (int) width: (int) height {
	GRand *pRand = g_rand_new();
	node->x = g_rand_int_range(pRand, 
			([node size] / 2), width - ([node size] / 2));
	node->y = g_rand_int_range(pRand, 
			([node size] / 2), height - ([node size] / 2));
	g_rand_free(pRand);
	return self;
}

+ edgesCircular: (Graph *) graph {
	int i = 0;
	List *list = [graph nodeList];

	while (i < [list size]) {
		if (i == [list size] - 1)
			[graph addEdge: [[[[Edge alloc] init] 
					pointA: [list getItem: i]] 
					pointB: [list getItem: 0]]];
		else
			[graph addEdge: [[[[Edge alloc] init] 
					pointA: [list getItem: i]] 
					pointB: [list getItem: i + 1]]];

		i++;
	}
	return self;
}

+ edgesCentered: (Graph *) graph {
	int i = 1;
	List *list = [graph nodeList];

	while (i < [list size]) {
		[graph addEdge: [[[[Edge alloc] init]
				pointA: [list getItem: 0]]
				pointB: [list getItem: i]]];
		i++;
	}
	return self;
}

+ edgesInterconnected: (Graph *) graph {
	List *list = [graph nodeList];
	int i = 0, j = 0;
	int list_size = [list size];

	while (i < list_size) {
		j = i;
		while (j < list_size) {
			[graph addEdge: [[[[Edge alloc] init]
					pointA: [list getItem: i]]
					pointB: [list getItem: j]]];
			j++;
		}
		i++;
	}
	return self;
}

+ edgesBinaryTree: (Graph *) graph {
	List *list = [graph nodeList];
	int i = 0, cur_index = 0;
	int list_size = [list size];

	while (i < list_size) {
		cur_index = (2 * i);
		if (cur_index + 1 > list_size - 1)
			break;
		else
			[graph addEdge: [[[[Edge alloc] init]
					 pointA: [list getItem: i]]
					 pointB: [list getItem: cur_index + 1]]];

		if (cur_index + 2 > list_size - 1)
			break;
		else
			[graph addEdge: [[[[Edge alloc] init]
					pointA: [list getItem: i]]
					pointB: [list getItem: cur_index + 2]]];
		i++;
	}
	return self;
}

@end

