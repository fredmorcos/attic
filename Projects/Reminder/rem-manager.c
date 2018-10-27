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

#include "rem-manager.h"

#include <stdlib.h>
#include <time.h>
#include <stddef.h>
#include <assert.h>

void manager_generic_task_free(Object);
void manager_run_alarm(Task);

Manager manager_new()
{
	Manager tmp_manager;

	tmp_manager = malloc(sizeof(struct manager_t));
	tmp_manager->tasks = list_new();

	return tmp_manager;
}

void manager_free(Manager manager)
{
	assert(manager != NULL);

	list_free(manager->tasks, manager_generic_task_free);
	free(manager);
}

void manager_generic_task_free(Object task)
{
	assert(task != NULL);

	Task tmp_task;

	tmp_task = task;
	task_free(tmp_task);
}

void manager_add_task(Manager manager, Task task)
{
	assert(manager != NULL);
	assert(task != NULL);

	list_add(manager->tasks, task);
}

void manager_run_alarms(Manager manager)
{
	assert(manager != NULL);

	List iter;
	Task task;
	time_t cur_time;

	cur_time = time(NULL);

	iter = manager->tasks;
	while (iter)
	{
		task = iter->data;

		if (task->expiry <= cur_time && !task->alarmed)
			manager_run_alarm(task);

		iter = iter->next;
	}
}

bool manager_all_tasks_expired(Manager manager)
{
	assert(manager != NULL);

	List iter;
	Task task;

	iter = manager->tasks;

	while (iter)
	{
		task = iter->data;
		if (!task->alarmed)
			return false;

		iter = iter->next;
	}

	return true;
}

// TODO
#include <stdio.h>

void manager_run_alarm(Task task)
{
	assert(task != NULL);

	puts(task->text);
	task->alarmed = true;
}

