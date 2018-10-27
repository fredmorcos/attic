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

#include <unistd.h>
#include <stdio.h>

#include "rem-manager.h"
#include "rem-task.h"
#include "rem-utils.h"

int main(int argc, StringList argv)
{
	Manager manager;

	manager = manager_new();
	manager_add_task(manager, task_new("task1", time(NULL) + 2));
	manager_add_task(manager, task_new("task2", time(NULL) + 4));
	manager_add_task(manager, task_new("task3", time(NULL) + 5));
	manager_add_task(manager, task_new("task4", time(NULL) + 7));

	while (1)
	{
		manager_run_alarms(manager);
		if (manager_all_tasks_expired(manager))
			break;
		sleep(5);
	}

	manager_free(manager);
	return 0;
}

