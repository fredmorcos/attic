/*
 * This file is part of heksniff
 * 
 * Hex packet sniffer
 * Copyright (C) 2007  Frederic-Gerald Morcos
 * 
 * heksniff is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * heksniff is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with heksniff.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "extra.h"
#include "global.h"
#include "session.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

void showHelp (void);

void parseArgs (int argc, char *argv [], Session *s) {
	int i = 0;
	while (i < argc) 
	{
		if (strcmp (argv [i], "--help") == 0) 
		{
			showHelp ();
		}
		else if (strcmp (argv [i], "-i") == 0)
		{
			if (i + 1 == argc) 
			{
				stderrQuit ("No interface given.\n", s);
			}
			else 
			{
				s->interface = argv [++i];
			}
		}
		else if (strcmp (argv [i], "-w") == 0)
		{
			if (i + 1 == argc) 
			{
				stderrQuit ("No width given.\n", s);
			}
			else
			{	
				s->settings->wid = atoi (argv [++i]);
			}
		}
		else if (strcmp (argv [i], "-o") == 0) 
		{
			if (i + 1 == argc) 
			{
				stderrQuit ("No filename given.\n", s);
			}
			else 
			{
				s->settings->file = TRUE;
				s->settings->filename = argv [++i];
			}
		}
		else if (strcmp (argv [i], "-q") == 0)
		{
			s->settings->std = FALSE;
		}
		
		i++;
	}
}

void stderrQuit (char *msg, Session *s) 
{
	fprintf (stderr, msg);
	
	if (s->handle != NULL)
		pcap_close (s->handle);
	
	closeSession (s);
	exit (EXIT_FAILURE);
}

void showHelp () 
{
	printf (
"Heksniff Packet Sniffer %.1f\n\
Fred Morcos 2007-2008 - GPLv3\n\n\
Usage:\n\
--help\t\tShow this help\n\
-i <dev>\tForce sniffing on <dev>. Default is any device.\n\
-w <width>\tForce a console output width. Default is 80 characters.\n\
-o <file>\tTODO: Enable output to file. Default is disabled.\n\
-q \t\tDisables all console output.\n", HSVER);
	
	exit (EXIT_SUCCESS);
}
