#ifndef __GRAPH_GEN_H__
#define __GRAPH_GEN_H__

#include "graph.h"

void graph_generate_star(graph_t *, int, int, int, int, int);
void graph_generate_random_particles(graph_t *, int, int, int, int, int);
vertex_t *graph_generate_vertex(int, int, int, int);

#endif /* __GRAPH_GEN_H__ */
