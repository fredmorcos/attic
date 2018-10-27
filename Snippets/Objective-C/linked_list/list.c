#include "list.h"
#include <stdlib.h>

ListItem *list_item_new(void *);

List *list_new()
{
	List *temp = (List *)malloc(sizeof(List));
	temp->first = NULL;
	temp->last = NULL;
	temp->size = 0;
	return temp;
}

void list_append(List *plist, void *pdata)
{
	/* if list is empty, first and last point to the same place */
	if (plist->first == NULL)
	{
		plist->first = list_item_new(pdata);
		plist->last = plist->first;
	}
	else
	{
		plist->last = plist->last->next;
		plist->last = list_item_new(pdata);
	}
	plist->size++;
}

ListItem *list_item_new(void *pdata)
{
	ListItem *temp = (ListItem *)malloc(sizeof(ListItem));
	temp->next = NULL;
	temp->data = pdata;
	return temp;
}
