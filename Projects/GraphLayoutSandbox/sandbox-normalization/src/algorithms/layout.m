#import "layout.h"
#import <graph.h>
#import <node.h>
#import <point.h>
#import <math.h>
#import <stdlib.h>

#define DEBUG

#ifdef DEBUG
#import <stdio.h>
#endif

@implementation Layout

/* algorithm for one run of the force-based layout:
 * c = electrostatic const (1/(4 PI E0))
 * k = spring constant
 * t = timestep
 * d = damping
 * g = graph to run the algorithm on
 */
+ (Point *) forceLayout: (double) c: (double) k: (double) t: (double) d: (Graph *) g {
	List *nodelist = [g nodeList];
	List *edgelist = [g edgeList];
	int size = [nodelist size];
	int esize = [edgelist size];
	int width = [g width];
	int height = [g height];
	Node *cur_node, *oth_node;
	Gravity *gravity;
	Edge *cur_edge;
	Point *netforce, *velocity, *result;
	int i = 0, j = 0, nsize = 0;
	Point *energy = [[Point alloc] init];

	static int iterations = 0;
	static int calculations = 0;

	i = 0;
	while (i < size) {
		cur_node = [nodelist getItem: i];
		if ([cur_node lock]) {
			i++;
			continue;
		}
		netforce = [cur_node netforce];
		velocity = [cur_node velocity];
		[[netforce x: 0] y: 0];
		nsize = [cur_node size];

		/* charge repulsion */
/*		j = 0;
		while (j < size) {
			oth_node = [nodelist getItem: j];
			if (cur_node != oth_node) {
				result = [Layout coulrep: c: cur_node: oth_node];
				[[netforce x: [netforce x] + [result x]] 
							 y: [netforce y] + [result y]];
				[result free];
			}

			j++;
		}
*/

		j = 0;
		while (YES) {
			if (j >= size && j >= esize) 
				break;

			/* repulsion */
			if (j < size) {
				oth_node = [nodelist getItem: j];
				if (cur_node != oth_node) { // && 
//					[Layout distance: cur_node: oth_node] < [g width] * [g height] / (size * size)) {
//						[Layout distance: cur_node: oth_node] < 
//							sqrt(([g width] * [g height]) / size)) {
					result = [Layout coulrep: c: cur_node: oth_node];
					[[netforce x: [netforce x] + [result x]] 
							   y: [netforce y] + [result y]];
					[result free];

					calculations++;
				}
			}

			/* attraction */
			if (j < esize) {
				cur_edge = [edgelist getItem: j];
				if (cur_node == [cur_edge pointA] ||
						cur_node == [cur_edge pointB]) {
					result = [Layout hookeatt: k: cur_node: cur_edge];
					[[netforce x: [netforce x] + [result x]]
							   y: [netforce y] + [result y]];
					[result free];
				}
			}

			/* gravity trick */
			gravity = [g gravityNode];
			if ([Layout distance: cur_node: gravity] < [gravity size])
				[g gravity: [gravity size]: NO];

			if ([g gravity]) {
				result = [Layout coulrep: c: cur_node: gravity];
				[[netforce x: [netforce x] + [result x]]
						   y: [netforce y] + [result y]];
				[result free];
			}

			iterations++;

			// g_print("%d,%d\n", iterations, calculations);

			j++;
		}

		/* spring attraction */
/*	j = 0;
		while (j < [edgelist size]) {
			cur_edge = [edgelist getItem: j];
			if (cur_node == [cur_edge pointA] ||
					cur_node == [cur_edge pointB]) {
				result = [Layout hookeatt: k: cur_node: cur_edge];
				[[netforce x: [netforce x] + [result x]]
							 y: [netforce y] + [result y]];
				[result free];
			}
			j++;
		}
*/
		/* node velocity */
		[[velocity x: ([velocity x] + (t * [netforce x])) * d]
				   y: ([velocity y] + (t * [netforce y])) * d];

		/* node position */
		[[cur_node x: [cur_node x] + (t * [velocity x])] 
				   y: [cur_node y] + (t * [velocity y])];

		if ([g wall] == YES) {
			if ([cur_node x] < nsize) [cur_node x: nsize];
			if ([cur_node y] < nsize) [cur_node y: nsize];
			if ([cur_node x] > width - nsize) [cur_node x: width - nsize];
			if ([cur_node y] > height - nsize) [cur_node y: height - nsize];
		}

		[energy x: [energy x] + ([cur_node mass] * pow([velocity x], 2) / 2)];
		[energy y: [energy y] + ([cur_node mass] * pow([velocity y], 2) / 2)];

		i++;
	}

	return energy;
}

/* hooke attraction vector components for x and y directions */
+ (Point *) hookeatt: (double) k: (Node *) n: (Edge *) e {
	Point *a;
	Point *b;
	if (n == [e pointA]) {
		a = [e pointA];
		b = [e pointB];
	}
	else {
		b = [e pointA];
		a = [e pointB];
	}

/*	double denom = sqrt(pow([a x] - [b x], 2) + pow([a y] - [b y], 2));
	double forcex = (-k * ([a x] - [b x])) / denom;
	double forcey = (-k * ([a y] - [b y])) / denom;
*/

	double forcex = -k * ([a x] - [b x]);
	double forcey = -k * ([a y] - [b y]);

	return [[[[Point alloc] init] x: forcex] y: forcey];

/*	double force = [Layout hooke_scalar: k: e];
	double angle;
*/
	/* used to get the direction of the force, switches 
	 * between current and origin nodes and from/to
	 */
/*	if (n == [e pointA])
		angle = [Layout theta: [e pointB]: [e pointA]];
	else if (n == [e pointB])
		angle = [Layout theta: [e pointA]: [e pointB]];

	return [[[[Point alloc] init] x: force * cos(angle)] y: force * sin(angle)];
*/
}

/* scalar value of the attraction force
 * NOTE: assumes the spring's natural energy position = 0 
 */
/*
+ (double) hooke_scalar: (double) k: (Edge *) e {
	return k * [Layout distance: [e pointA]: [e pointB]];
}
*/

/* coulomb repulsion vector components in x and y directions */
+ (Point *) coulrep: (double) c: (Node *) n1: (Node *) n2 {

	double masses = [n1 mass] * [n2 mass];
	double dx = [n1 x] - [n2 x];
	double dy = [n1 y] - [n2 y];
	double denom = pow((pow(dx, 2) + pow(dy, 2)), (3 / 2));
	double numer = masses * c;
	double forcex = (numer * dx) / denom;
	double forcey = (numer * dy) / denom;

	return [[[[Point alloc] init] x: forcex] y: forcey];

/*	double force = [Layout coul_scalar: c: n1: n2];
	double angle = [Layout theta: n1: n2];
	return [[[[Point alloc] init] x: force * cos(angle)] y: force * sin(angle)];
*/
}

/* the scalar value of the repulsion force */
/*
+ (double) coul_scalar: (double) c: (Node *) n1: (Node *) n2 {
	int charges = n1->mass * n2->mass;
	int distance = [Layout distance: n1: n2];
	if (distance == 0)	*/	/* if distance is 0 then the force is infinite */
/*		return 0.5;
	return c * (charges / (distance * distance));
}
*/

/* theta = invtan (b / a) */
/* + (double) theta: (Point *) cur: (Point *) origin {
	int b = [cur y] - [origin y];
	int a = [cur x] - [origin x];
	if (a == 0) {	*/	/* infinity */
/*		if (b >= 0)
			return 90.0;
		else	*/	/* negative infinity */
/*			return 270.0;
	}		*/			/* every other number in between */
/*	return atan(b / a);
}
*/

+ (int) distance: (Point *) n1: (Point *) n2 {
	int dx = [n1 x] - [n2 x];
	int dy = [n1 y] - [n2 y];
	return dx + dy;
}

/* distance between two points */
/* + (int) distance: (Point *) n1: (Point *) n2 {
	int dx = [n1 x] - [n2 x];
	int dy = [n1 y] - [n2 y];
	return sqrt((dx * dx) + (dy * dy));
}
*/

@end

