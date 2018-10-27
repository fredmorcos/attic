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

#import <objc/Object.h>
#import "config.h"

typedef struct _ListItem {
	void *data;
#ifdef LINKEDLIST
	struct _ListItem *next;
#endif
} ListItem;

@interface List: Object {
@protected
	unsigned int size;
#ifdef LINKEDLIST
	ListItem *first,
			 *last;
#else
	ListItem *array;
	unsigned int allocSize;
#endif
}

- (const unsigned int) size;

- add: (void *) data;
- (void *) getFromIndex: (unsigned int) index;

- free;
- freeDeep;

@end

