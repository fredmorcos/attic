#include "graph.h"
#include "list.h"
#include <stdlib.h>
#include <assert.h>

vertex_t *
vertex_new()
{
  vertex_t *vertex = (vertex_t *) malloc(sizeof(vertex_t));

  assert(vertex);

  vertex->x = 0;
  vertex->y = 0;

  return vertex;
}

inline void
vertex_free(void *vertex)
{
  assert(vertex);
  free((vertex_t *)vertex);
}

edge_t *
edge_new()
{
  edge_t *edge = (edge_t *) malloc(sizeof(edge_t));

  assert(edge);

  edge->src = NULL;
  edge->dst = NULL;

  return edge;
}

inline void
edge_free(void *edge)
{
  assert(edge);
  free((edge_t *)edge);
}

graph_t *
graph_new()
{
  graph_t *graph = (graph_t *) malloc(sizeof(graph_t));

  assert(graph);

  graph->vertices = list_new();
  graph->edges = list_new();
  
  return graph;
}

void
graph_add_vertex(graph_t *graph, vertex_t *vertex)
{
  assert(graph);
  assert(vertex);

  list_append(graph->vertices, vertex);
}

void
graph_add_edge(graph_t *graph, edge_t *edge)
{
  assert(graph);
  assert(edge);

  list_append(graph->edges, edge);
}

void
graph_free(graph_t *graph)
{
  assert(graph);

  list_free_with_data(graph->vertices, vertex_free);
  list_free_with_data(graph->edges, edge_free);
  free(graph);
}
