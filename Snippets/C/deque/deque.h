#ifndef __DEQUE_H__
#define __DEQUE_H__

typedef struct _list_t
{
  void *data;
  struct _list_t *prev, *next;
} list_t;

typedef struct _deque_t
{
  list_t *left, *right;
  int size;
} deque_t;

deque_t *deque_new();
void deque_push_left(deque_t *, void *);
void deque_push_right(deque_t *, void *);
void *deque_pop_left(deque_t *);
void *deque_pop_right(deque_t *);
void deque_rotate_left(deque_t *);
void deque_rotate_right(deque_t *);
void deque_free(deque_t *);
void deque_print(deque_t *);

#endif /* __DEQUE_H__ */
