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

#include <stdbool.h>

#include "rem-list.h"
#include "rem-task.h"
#include "rem-utils.h"

struct manager_t
{
	struct list_t *tasks;
};

typedef struct manager_t * Manager;

Manager manager_new();
void manager_free(Manager);
void manager_add_task(Manager, Task);
void manager_run_alarms(Manager);
bool manager_all_tasks_expired(Manager);

