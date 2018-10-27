#include "vertex.h"

#include <stdlib.h>
#include <assert.h>
#include <stddef.h>

Vertex *vertex_new(void *data) {
	Vertex *tmp;

	tmp = malloc(sizeof(Vertex));
	tmp->data = data;

	return tmp;
}

void vertex_free_deep(Vertex *vertex, void (*dtor)(void *)) {
	assert(vertex != NULL);

	if (dtor != NULL)
		dtor(vertex->data);

	vertex_free(vertex);
}

