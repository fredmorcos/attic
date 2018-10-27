#ifndef __LIST_H__
#define __LIST_H__

struct _ListItem
{
	void *data;
	struct _ListItem *next;
};

typedef struct _ListItem ListItem;

typedef struct _List
{
	int size;
	ListItem *first;
	ListItem *last;
} List;

List *list_new();
void list_append(List *, void *);

#endif /* __LIST_H__ */
