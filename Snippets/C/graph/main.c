#include "graph.h"
#include <stdio.h>
#include <stdlib.h>

#define SIZE 200000

int main (int argc, char *argv[]) {
	int i = 0;
	Graph *x = (Graph *) malloc(sizeof(Graph));
	Graph *next = x;

	while (i < SIZE) {
		next->next = (Graph *) malloc(sizeof(Graph));
		next = next->next;
		i++;
	}

	printf("graph size: %d\n", count(x));

	return 0;
}

int count (Graph *x) {
	int i = 0;

	x->passed = 1;

	if (x->next != NULL && x->next->passed == 0)
		i = count(x->next);

	x->passed = 0;

	return 1 + i;
}

