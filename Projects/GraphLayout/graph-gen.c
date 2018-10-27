#include "graph-gen.h"
#include "config.h"
#include <stdlib.h>

void
graph_generate_star(graph_t *graph, int num_nodes, int min_x, int min_y, int max_x, int max_y)
{
  vertex_t *v, *v_first;
  edge_t *e;
  int i;

  v_first = graph_generate_vertex(min_x, min_y, max_x, max_y);
  graph_add_vertex(graph, v_first);

  for (i = 1; i < num_nodes; i++)
    {
      v = graph_generate_vertex(min_x, min_y, max_x, max_y);
      e = edge_new();
      e->src = v_first;
      e->dst = v;
      graph_add_vertex(graph, v);
      graph_add_edge(graph, e);
    }
}

void
graph_generate_random_particles(graph_t *graph, int num_nodes, int min_x, int min_y, int max_x, int max_y)
{
  vertex_t *v;
  int i;

  for (i = 0; i < num_nodes; i++)
    {
      v = graph_generate_vertex(min_x, min_y, max_x, max_y);
      graph_add_vertex(graph, v);
    }
}

vertex_t *
graph_generate_vertex(int min_x, int min_y, int max_x, int max_y)
{
  vertex_t *v = vertex_new();
  int x = rand() % (max_x - (NODE_RADI * 2)) + (min_x + NODE_RADI),
      y = rand() % (max_y - (NODE_RADI * 2)) + (min_y + NODE_RADI);

  v->x = x;
  v->y = y;
  return v;
}
