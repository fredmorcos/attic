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

#pragma once

#include "rem-utils.h"

struct list_t
{
	struct list_t *next;
	Object data;
};

typedef struct list_t * List;

List list_new();
void list_free(List, void (*)(Object));
void list_add(List, const Object);

