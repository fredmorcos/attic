#include "deque.h"
#include <stdlib.h>

deque_t *
deque_new()
{
  deque_t *d = (deque_t *) malloc(sizeof(deque_t));
  d->_data = (void *) malloc(sizeof(void *) * CHUNK);
  d->_left = CHUNK / 2;
  d->_right = d->_left + 1;
  d->_rsize = CHUNK;
  return d;
}

void
deque_push_left(deque_t *d, void *item)
{
  if (d->_left < 0)
    {
      void **nd = (void *) malloc(sizeof(void *) * (d->_rsize + CHUNK));
      int i;

      for (i = d->_left + 1; i < d->_right; i++)
	nd[CHUNK + i] = d->_data[i];

      d->_left = CHUNK;
      d->_right += CHUNK;

      d->_rsize += CHUNK;

      free(d->_data);
      d->_data = nd;
      nd = NULL;
    }

  d->_data[d->_left--] = item;
}

void
deque_push_right(deque_t *d, void *item)
{
  if (d->_right == d->_rsize)
    {
      void **nd = (void *) malloc(sizeof(void *) * (d->_rsize + CHUNK));
      int i;

      for (i = d->_left + 1; i < d->_right; i++)
	nd[CHUNK + i] = d->_data[i];

      d->_rsize += CHUNK;

      free(d->_data);
      d->_data = nd;
      nd = NULL;
    }

  d->_data[d->_right++] = item;
}

void *
deque_pop_left(deque_t *d)
{
  d->_left++;
  return d->_data[d->_left];
}

void *
deque_pop_right(deque_t *d)
{
  d->_right--;
  return d->_data[d->_right];
}

int
deque_is_empty(deque_t *d)
{
  return (deque_size(d) == 0);
}

inline int
deque_size(deque_t *d)
{
  return (d->_right - d->_left) - 1;
}

void
deque_free(deque_t *d, void (*free_func)(void *))
{
  if (free_func)
    {
      int i;
      for (i = d->_left + 1; i < d->_right; i++)
	  free_func(d->_data[i]);
    }

  free(d->_data);
  free(d);
}

#include <stdio.h>

void
deque_print(deque_t *d)
{
  int i;
  for (i = d->_left + 1; i < d->_right; i++)
    {
      if (d->_data[i] == NULL) printf("fooo %d\n", i);
      printf("%d ", (int)(*((int*)d->_data[i])));
    }
  puts("");
}
