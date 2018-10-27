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

#include <stdio.h>
#include <stdlib.h>
#include <pcap.h>

#include "global.h"
#include "session.h"
#include "extra.h"

void gotPacket (u_char *args, const struct pcap_pkthdr *header, const u_char *packet);

/* excuse this, for now, pcap sucks enough 
	to not give extra parameter passing to 
	it's callback functions
*/
Session *curSes;

int main (int argc, char *argv [])
{
	curSes = newSession ();	
	parseArgs (argc, argv, curSes);
	
	if (getuid () != 0) stderrQuit ("You have to be root!\n", curSes);
	
	openSession (curSes);
	curSes->onPacketArrive = gotPacket;
	startSession (curSes);
	
	closeSession (curSes);
	return EXIT_SUCCESS;
}

void gotPacket (u_char *args, const struct pcap_pkthdr *header, const u_char *packet)
{
	int i = 0, j = 1;
	
	if (curSes->settings->std == TRUE)
		printf ("\n---PACKET START: Size: %d bytes ---\n", header->len);
	
	while (i < header->len) 
	{
		if (curSes->settings->std == TRUE)
			printf ("0x%x", packet [i]);
		
		if (packet [i] < 16)
			if (curSes->settings->std == TRUE)
				printf (" ");

		if (j >= (curSes->settings->wid / 5))
		{
			if (curSes->settings->std == TRUE)
				printf ("\n");
			j = 0;
		}
		else
			if (curSes->settings->std == TRUE)
				printf (" ");
		
		i++;
		j++;
	}
	
	if (curSes->settings->std == TRUE)
		printf ("\n---PACKET END---\n");
}
