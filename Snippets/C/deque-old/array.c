#include "array.h"
#include <stdlib.h>

#define ARRAY_CHUNK 20

#define SANITIZE(array, item) do {		\
    if (!(array->length))			\
      {						\
	array_add_first_item(array, item);	\
	return;					\
      }						\
    						\
    if (array->length == array->_real_length)	\
      array_extend(array);			\
  } while (0);

#define INSERT(array, item, index) do {			\
    int len = array->length;				\
    for (; len >= index; len--)				\
      array->_data[len + 1] = array->_data[len];	\
    array->_data[len] = item;				\
    array->length++;					\
  } while (0);

#define DELETE(array, index) do {			\
    void *data = array->_data[index];			\
    for (; index < array->length; index++)		\
      array->_data[index] = array->_data[index + 1];	\
    array->length--;					\
    return data;					\
  } while (0);

void array_extend(array_t *);

array_t *
array_new()
{
  array_t *tmp = (array_t *) malloc(sizeof(array_t));
  tmp->_data = NULL;
  tmp->length = 0;
  tmp->_real_length = 0;
  return tmp;
}

int
array_index_with_data(array_t *array, void *data)
{
  int index = 0;

  while (index < array->length)
    {
      if (array->_data[index] == data) return index;
      index++;
    }

  return -1;
}

inline void *
array_item_at_index(array_t *array, int index)
{
  return array->_data[index];
}

void
array_extend(array_t *array)
{
  array->_data = (void *) realloc(array->_data, sizeof(void *) * (array->_real_length + ARRAY_CHUNK));
  array->_real_length += ARRAY_CHUNK;
}

void
array_add_first_item(array_t *array, void *item)
{
  array->_data = (void **) malloc(sizeof(void *) * ARRAY_CHUNK);
  array->_data[0] = item;
  array->_real_length += ARRAY_CHUNK;
  array->length++;
}

void
array_prepend(array_t *array, void *item)
{
  SANITIZE(array, item);
  INSERT(array, item, 0);
}

void
array_append(array_t *array, void *item)
{
  SANITIZE(array, item);
  array->_data[array->length++] = item;
}

void
array_insert(array_t *array, void *item, int index)
{
  SANITIZE(array, item);
  INSERT(array, item, index);
}

void *
array_remove_index(array_t *array, int index)
{
  DELETE(array, index);
}

inline void *
array_remove_item(array_t *array, void *item)
{
  return array_remove_index(array, array_index_with_data(array, item));
}

#include <stdio.h>

void
array_free(array_t *array, void (*free_func)(void *))
{
  printf("%d\n", array->length);
  printf("%d\n", array->_real_length);
  int i = 0;
  while (i < array->length)
    {
      if (free_func) free_func(array->_data[i]);
      i++;
    }

  free(array->_data);
  free(array);
}
