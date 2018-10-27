#include "list.h"
#include <stdlib.h>

node_t *list_node_at_index(list_t *, int);
node_t *list_node_with_data(list_t *, void *);

list_t *
list_new()
{
  list_t *tmp = (list_t *) malloc(sizeof(list_t));
  tmp->first = NULL;
  tmp->last = NULL;
  tmp->length = 0;
  return tmp;
}

node_t *
list_node_at_index(list_t *list, int index)
{
  node_t *current_node = list->first;

  for (; index >= 0; index--)
    current_node = current_node->next;

  return current_node;
}

int
list_index_with_data(list_t *list, void *data)
{
  int index = 0;
  node_t *current_node = list->first;

  while (1)
    {
      if (current_node->data == data) break;
      current_node = current_node->next;
      index++;
    }

  return index;
}

node_t *
list_node_with_data(list_t *list, void *data)
{
  node_t *current_node = list->first;

  while (1)
    {
      if (current_node->data == data) break;
      current_node = current_node->next;
    }

  return current_node;
}

inline void *
list_item_at_index(list_t *list, int index)
{
  return list_node_at_index(list, index)->data;
}

void
list_add_first_item(list_t *list, void *item)
{
  node_t *tmp = (node_t *) malloc(sizeof(node_t));
  tmp->data = item;
  tmp->prev = NULL;
  tmp->next = NULL;
  list->first = tmp;
  list->last = tmp;
  list->length++;
}

void
list_prepend(list_t *list, void *item)
{
  if (!(list->length))
    {
      list_add_first_item(list, item);
      return;
    }

  node_t *tmp = (node_t *) malloc(sizeof(node_t));
  tmp->data = item;
  tmp->prev = NULL;
  tmp->next = list->first;
  list->first = tmp;
  list->length++;
}

void
list_append(list_t *list, void *item)
{
  if (!(list->length))
    {
      list_add_first_item(list, item);
      return;
    }

  node_t *tmp = (node_t *) malloc(sizeof(node_t));
  tmp->data = item;
  tmp->prev = list->last;
  tmp->prev->next = tmp;
  tmp->next = NULL;
  list->last = tmp;
  list->length++;
}

void
list_insert(list_t *list, void *item, int index)
{
  if (!(list->length))
    {
      list_add_first_item(list, item);
      return;
    }  

  node_t
    *node = list_node_at_index(list, index),
    *tmp = (node_t *) malloc(sizeof(node_t));
  tmp->data = item;
  tmp->prev = node->prev;
  tmp->next = node;
  node->prev = tmp;
  tmp->prev->next = tmp;
  list->length++;
}

void *
list_remove_index(list_t *list, int index)
{
  node_t *node = list_node_at_index(list, index);
  void *data = node->data;
  node->prev->next = node->next;
  node->next->prev = node->prev;
  free(node);
  list->length--;
  return data;
}

inline void *
list_remove_item(list_t *list, void *item)
{
  return list_remove_index(list, list_index_with_data(list, item));
}

void
list_free(list_t *list, void (*free_func)(void *))
{
  node_t 
    *node = list->first,
    *next_node;

  while (node)
    {
      next_node = node->next;
      if (free_func)
	free_func(node->data);
      free(node);
      node = next_node;
    }

  free(list);
}
