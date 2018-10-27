#include "deque.h"
#include <stdio.h>
#include <stdlib.h>

list_t *list_new();
void deque_add_first(deque_t *, void *);

list_t *
list_new()
{
  list_t *tmp = (list_t *) malloc(sizeof(list_t));
  tmp->data = NULL;
  tmp->prev = NULL;
  tmp->next = NULL;
  return tmp;
}

deque_t *
deque_new()
{
  deque_t *tmp = (deque_t *) malloc(sizeof(deque_t));
  tmp->size = 0;
  tmp->left = NULL;
  tmp->right = NULL;
  return tmp;
}

void
deque_add_first(deque_t *deque, void *item)
{
  list_t *node = list_new();
  node->data = item;
  deque->left = node;
  deque->right = node;
  deque->size++;
}

void
deque_push_left(deque_t *deque, void *item)
{
  if (deque->size == 0)
    {
      deque_add_first(deque, item);
      return;
    }

  list_t *node = list_new();
  node->data = item;
  node->next = deque->left;
  deque->left->prev = node;
  deque->left = node;
  deque->size++;
}

void
deque_push_right(deque_t *deque, void *item)
{
  if (deque->size == 0)
    {
      deque_add_first(deque, item);
      return;
    }

  list_t *node = list_new();
  node->data = item;
  node->prev = deque->right;
  deque->right->next = node;
  deque->right = node;
  deque->size++;
}

void *
deque_pop_left(deque_t *deque)
{
  if (deque->size == 0) return NULL;

  void *data = deque->left->data;

  if (deque->size == 1)
    {
      free(deque->left);
      deque->left = NULL;
      deque->right = NULL;
    }
  else
    {
      deque->left = deque->left->next;
      free(deque->left->prev);
      deque->left->prev = NULL;
    }

  deque->size--;
  return data;
}

void *
deque_pop_right(deque_t *deque)
{
  if (deque->size == 0) return NULL;

  void *data = deque->right->data;

  if (deque->size == 1)
    {
      free(deque->right);
      deque->left = NULL;
      deque->right = NULL;
    }
  else
    {
      deque->right = deque->right->prev;
      free(deque->right->next);
      deque->right->next = NULL;
    }

  deque->size--;
  return data;
}

void
deque_rotate_left(deque_t *deque)
{
  list_t *node = deque->left;

  deque->left = deque->left->next;
  deque->left->prev = NULL;
  node->next = NULL;
  deque->right->next = node;
  node->prev = deque->right;
  deque->right = node;
}

void
deque_rotate_right(deque_t *deque)
{
  list_t *node = deque->right;

  deque->right = deque->right->prev;
  deque->right->next = NULL;
  node->prev = NULL;
  deque->left->prev = node;
  node->next = deque->left;
  deque->left = node;
}

void
deque_free(deque_t *deque)
{
  void *item;

  while ((item = deque_pop_left(deque)))
    free(item);
  free(deque);
}

void
deque_print(deque_t *deque)
{
  list_t *node = deque->left;

  while (node)
    {
      printf("%d ", (int)(*(int *)node->data));
      node = node->next;
    }

  puts("");
}
