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

#include "session.h"
#include "settings.h"
#include "extra.h"

#include <stdio.h>
#include <stdlib.h>
#include <pcap.h>

inline Session *newSession ()
{
	Session *tmpSession = malloc (sizeof (Session));
	
	tmpSession->handle = NULL;
	tmpSession->interface = NULL;
	tmpSession->settings = newSettings ();
	tmpSession->onPacketArrive = NULL;
	
	return tmpSession;
}

void openSession (Session *s)
{
	char errBuffer [PCAP_ERRBUF_SIZE];
	
	if (s->interface == NULL)
		s->interface = pcap_lookupdev (errBuffer);
	
	if (s->interface == NULL)
	{
		fprintf (stderr, "Error: %s\n", errBuffer);
		stderrQuit ("No usable device interface.\n", s);
	}
	
	s->handle = pcap_open_live (s->interface, BUFSIZ, 1, 1000, errBuffer);
	
	if (s->handle == NULL)
	{
		fprintf (stderr, "Error: %s\n", errBuffer);
		stderrQuit ("Cannot sniff on device interface.\n", s);
	}
	
	if (s->settings->std == TRUE)
		printf ("Session opened on device %s\n", s->interface);
}

void startSession (Session *s)
{
	pcap_setnonblock (s->handle, 0, s->interface);
	
	if (s->onPacketArrive == NULL)
		stderrQuit ("HS_ERROR: No packet arrival callback function set!\n", s);
	
	pcap_loop (s->handle, -1, s->onPacketArrive, NULL);
}

inline void closeSession (Session *s)
{
	if (s->handle != NULL)
		pcap_close (s->handle);
	
	closeSettings (s->settings);
	free (s);
}
