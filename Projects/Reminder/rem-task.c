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

#include "rem-task.h"

#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

Task task_new(const String text, const time_t expiry)
{
	assert(text != NULL);
	assert(expiry > time(NULL));

	Task tmp_task;

	tmp_task = malloc(sizeof(struct task_t));
	tmp_task->text = text;
	tmp_task->expiry = expiry;
	tmp_task->alarmed = false;

	return tmp_task;
}

void task_free(Task task)
{
	assert(task != NULL);

	free(task);
}

