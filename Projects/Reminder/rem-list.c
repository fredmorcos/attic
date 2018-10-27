/*
 *	This file is part of Fred's Reminder.
 *
 *	Fred's Reminder is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Fred's Reminder is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Fred's Reminder.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "rem-list.h"

#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

List list_new()
{
	List list;

	list = malloc(sizeof(struct list_t));
	list->next = NULL;
	list->data = NULL;

	return list;
}

void list_free(List list, void (*data_dtor)(Object))
{
	assert(list != NULL);

	List iter,
	     tmp_iter;

	iter = list;
	while (iter)
	{
		if (data_dtor)
			data_dtor(iter->data);
		else
			free(iter->data);

		tmp_iter = iter;
		iter = iter->next;
		free(tmp_iter);
	}
}

void list_add(List list, const Object data)
{
	assert(data != NULL);

	List iter,
	     new_node;

	if (!list->data)
	{
		list->data = data;
		return;
	}

	iter = list;
	while (iter)
	{
		if (!iter->next)
			break;
		iter = iter->next;
	}

	new_node = malloc(sizeof(struct list_t));
	new_node->next = NULL;
	new_node->data = data;

	iter->next = new_node;
}

