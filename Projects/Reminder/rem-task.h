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

#include <time.h>
#include <stdbool.h>

#include "rem-utils.h"

struct task_t
{
	String text;
	time_t expiry;
	bool alarmed;
};

typedef struct task_t * Task;

Task task_new(const String, const time_t);
void task_free(Task);

