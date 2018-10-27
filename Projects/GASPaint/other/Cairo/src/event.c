/*
 * This file is part of gaspaint
 * 
 * Copyright (C) 2007-2009  Frederic-Gerald Morcos
 * 
 * gaspaint is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * gaspaint is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with gaspaint.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "event.h"
#include "public.h"

#include <string.h>
#include <stdlib.h>

/* add line to the event string */
void add_event (char *e)
{
	g_string_append (events, e);
	g_string_append (events, "\n");
}

/* will go through the events string and parse it to
 * understand which command/object is to be drawn with the certain
 * properties.
 */
void drawEvents ()
{
	char *events_copy = malloc (sizeof (char) * strlen (events->str) + 2);
	//strcpy (events_copy, events->str);
	//strcat (events_copy, "\0");
	
	char *tmp;
	int i=0, j;
	while ( 1 )
	{
		strcpy (events_copy, events->str);
		strcat (events_copy, "\0");
		// printf ( "Events String: %s###\n", events_copy );
		tmp = strtok ( events_copy, "\n" );
		j=0;
		while ( j<i )
		{
			tmp = strtok ( NULL, "\n" );
			// printf ( "Strings %d: %s#\n", j, tmp );
			j++;
		}
		if ( tmp == NULL ) break;
		drawCommand ( tmp );
		i++;
	}
	free ( events_copy );
}
