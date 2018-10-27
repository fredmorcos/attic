#include "list.h"
#include <stdlib.h>
#include <assert.h>

node_t *list_node_new();

list_t *
list_new()
{
  list_t *list = (list_t *) malloc(sizeof(list_t));

  assert (list);

  list->first = NULL;
  list->last = NULL;
  list->length = 0;

  return list;
}

node_t *
list_node_new()
{
  node_t *node = (node_t *) malloc(sizeof(node_t));

  assert(node);

  node->data = NULL;
  node->next = NULL;

  return node;
}

void
list_append(list_t *list, void *item)
{
  assert(list);
  assert(item);

  node_t *node = list_node_new();

  node->data = item;
  
  if (list->length)
    {
      list->last->next = node;
      list->last = node;
    }
  else
    {
      list->first = node;
      list->last = node;
    }

  list->length++;
}

void
list_free(list_t *list)
{
  assert(list);

  node_t *node = list->first,
         *next_node;

  while (node)
    {
      next_node = node->next;
      free(node);
      node = next_node;
    }

  free(list);
}

void
list_free_with_data(list_t *list, void (*free_func)(void *))
{
  assert(list);
  assert(free_func);

  node_t *node = list->first,
         *next_node;

  while (node)
    {
      next_node = node->next;
      free_func(node->data);
      free(node);
      node = next_node;
    }

  free(list);
}
