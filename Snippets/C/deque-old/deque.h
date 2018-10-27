#ifndef __DEQUE_H__
#define __DEQUE_H__

#define CHUNK 10

typedef struct _deque
{
  void **_data;
  int _left, _right, _rsize;
} deque_t;

deque_t *deque_new();
void deque_push_left(deque_t *, void *);
void deque_push_right(deque_t *, void *);
void *deque_pop_left(deque_t *);
void *deque_pop_right(deque_t *);
int deque_is_empty(deque_t *);
int deque_size(deque_t *);
void deque_free(deque_t *, void (*)(void *));
void deque_print(deque_t *);

#endif /* __DEQUE_H__ */
