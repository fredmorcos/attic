/*
	This file is part of cv3.

	Copyright (C) 2008	Frederic-Gerald Morcos <fred.morcos@gmail.com>

	cv3 is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	cv3 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with cv3.  If not, see <http://www.gnu.org/licenses/>.
*/

#import "List.h"
#import "config.h"
#import <stdlib.h>

@implementation List

/**
 * Returns the size of the list.
 */
- (const unsigned int) size {
	return size;
}

/**
 * Appends an item to the end of the list.
 */
- add: (void *) data {
#ifdef LINKEDLIST
	if (first && last) {
		last->next = malloc(sizeof(ListItem));
		last->next->data = data;
		last = last->next;
	}
	else {
		first = malloc(sizeof(ListItem));
		first->data = data;
		last = first;
	}
#else
	if (!array) {
		array = malloc(sizeof(ListItem) * 20);
		allocSize = 20;
	}

	if (size == allocSize) {
		array = realloc(array, (sizeof(ListItem) * (allocSize + 10)));
		allocSize += 10;
	}

	array[size].data = data;
#endif

	++size;
	return self;
}

/**
 * Return an item from its index in the list.
 */
- (void *) getFromIndex: (unsigned int) index {
#ifdef LINKEDLIST
	if (index >= size) return NULL;

	ListItem *tmp = first;
	for (; index > 0; index--)
		tmp = tmp->next;
	if (tmp)
		return tmp->data;
	return NULL;
#else
	if (index >= size) return NULL;
	return array[index].data;
#endif
}

/**
 * Deallocates the list without freeing 
 * the data items in the list.
 */
- free {
#ifdef LINKEDLIST
	ListItem *tmp = first,
			 *tmp2 = first->next;
	for (int i = 0; i < size; i++) {
		free(tmp);
		tmp = tmp2->next;
		free(tmp2);
	}
	return [super free];
#else
	free(array);
	return [super free];
#endif
}

/**
 * Frees the list _and_ the data items
 * in it.
 */
- freeDeep {
#ifdef LINKEDLIST
	ListItem *tmp = first,
			 *tmp2;
	for (int i = 0; i < size; i++) {
		tmp2 = tmp->next;
		free(tmp->data);
		free(tmp);
		tmp = tmp2;
	}
	return [super free];
#else
	for (int i = 0; i < size; i++)
		free(array[i].data);
	return [self free];
#endif
}

@end

