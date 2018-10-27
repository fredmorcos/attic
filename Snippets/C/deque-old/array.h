#ifndef __ARRAY_H__
#define __ARRAY_H__

typedef struct _array_t
{
  void **_data;
  int length, _real_length;
} array_t;

array_t *array_new();
void *array_item_at_index(array_t *, int);
int array_index_with_data(array_t *, void *);
void array_prepend(array_t *, void *);
void array_append(array_t *, void *);
void array_insert(array_t *, void *, int);
void *array_remove_index(array_t *, int);
void *array_remove_item(array_t *, void *);
void array_free(array_t *, void (*)(void *));

#endif /* __ARRAY_H__ */
