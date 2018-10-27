#ifndef __LIST_H__
#define __LIST_H__

typedef struct _node_t
{
  void *data;
  struct _node_t *next;
} node_t;

typedef struct _list_t
{
  node_t *first, *last;
  int length;
} list_t;

list_t *list_new();
void list_append(list_t *, void *);
void list_free(list_t *);
void list_free_with_data(list_t *, void (*)(void *));

#endif /* __LIST_H__ */
