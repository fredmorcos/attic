#ifndef __GRAPH_H__
#define __GRAPH_H__

#include "list.h"

typedef struct _vertex_t
{
  int x, y;
} vertex_t;

typedef struct _edge_t
{
  vertex_t *src, *dst;
} edge_t;

typedef struct _graph_t
{
  list_t *vertices, *edges;
} graph_t;

vertex_t *vertex_new();
void vertex_free(void *);

edge_t *edge_new();
void edge_free(void *);

graph_t *graph_new();
void graph_add_vertex(graph_t *, vertex_t *);
void graph_add_edge(graph_t *, edge_t *);
void graph_free(graph_t *);

#endif /* __GRAPH_H__ */
