#ifndef __LIST_H__
#define __LIST_H__

typedef struct _node_t
{
  void *data;
  struct _node_t *prev, *next;
} node_t;

typedef struct _list_t
{
  node_t *first, *last;
  int length;
} list_t;

list_t *list_new();
void *list_item_at_index(list_t *, int);
int list_index_with_data(list_t *, void *);
void list_prepend(list_t *, void *);
void list_append(list_t *, void *);
void list_insert(list_t *, void *, int);
void *list_remove_index(list_t *, int);
void *list_remove_item(list_t *, void *);
void list_free(list_t *, void (*)(void *));

#endif /* __LIST_H__ */
