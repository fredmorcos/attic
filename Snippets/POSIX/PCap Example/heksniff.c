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

/* references:
 * 		http://www.tcpdump.org/pcap.htm
 * 		pcap manpage
 */

#include <stdio.h>		/* printf (), stderr, etc... */
#include <pcap.h>		/* packet capturing library */

/* our callback function's prototype */
void got_packet (u_char *args, const struct pcap_pkthdr *header, const u_char *packet);

int main (int argc, char *argv [])
{
	char	*dev, 									/* device name */
			errbuf [PCAP_ERRBUF_SIZE];				/* error string */
			
	pcap_t	*handle;								/* our sniffing session handle (id) */
	
	struct 	pcap_pkthdr 	header;					/* the packet header */
	const 	u_char 			*packet;				/* the packet itself */
	
	// bpf_u_int32 	net;							/* local ip */
	// bpf_u_int32 	mask;							/* netmask */
	
	/* if pcap_lookupdev () doesn't return anything, try the "any" or NULL device name
	 * to capture on all interfaces (which is somehow required for our embedded device).
	 */
	dev = pcap_lookupdev (errbuf);					/* lookup the default network device */
	if (dev == NULL)
	{
		fprintf (stderr, "Error: %s\n", errbuf);
		return 2;
		/*
		printf ("Trying ALL interfaces...\n");
		*/
		
		/* try to get some information about the device/network (ip, netmask, etc...),
	 	 * if failed, then no problem, just set them to 0, they're not fatal properties anyways.
	 	 */
	 	/*
		if (pcap_lookupnet (dev, &net, &mask, errbuf) == -1)
		{
			fprintf(stderr, "Warning: %s\n", errbuf);
			net = 0;
			mask = 0;
		}
		*/
		
		/* NOTE: opening a session on "all"/NULL devices is only supported on linux 2.2 
		 * and higher. also, we enable promiscious mode so we receive as much packets on 
		 * the network as possible.
		 */
		/* 
		handle = pcap_open_live (NULL, BUFSIZ, 1, 1000, errbuf);
	 	if (handle == NULL)
	 	{
			fprintf (stderr, "Error: %s\nMake sure you are running as root.\n", errbuf);
			return 2;
		}
		*/
	}
	
	/* open a sniffing session, if not possible, then exit */
	handle = pcap_open_live (dev, BUFSIZ, 1, 1000, errbuf);
	if (handle == NULL)
	{
		fprintf (stderr, "Error: %s\nMake sure you are running as root.\n", errbuf);
		return 2;
	}
	
	printf ("Starting sniffing on device %s\n", dev);
	
	/* set non-blocking mode to off (should be off by default, but just in case).
	 * because pcap_loop () doesn't run in non-blocking mode
	 */
	pcap_setnonblock (handle, 0, dev);

	/* register the callback got_packet (), whenever a packet arrives, this function 
	 * will be called. also, only exit the loop if an error occurs.
	 */
	pcap_loop (handle, -1, got_packet, NULL);
	
	/* this will probably never be reached except if an error occurs */
	printf ("Something wrong happened. Closing.\n");
	pcap_close (handle);							/* close the session and free the handle */

	return 0;
}

/* the callback function used when any packet arrives,
 * here, args is ignored
 */
void got_packet (u_char *args, const struct pcap_pkthdr *header, const u_char *packet)
{
	int x = 0, y = 1;
	printf ("\n\n---PACKET BEGIN---\n\n");
	printf ("Size: %d bytes\n\n", header->len);
	
	/* go through the whole packet and print it */
	while (x < header->len) 
	{
		/* print each packet byte in hex */
		printf ("0x%x", packet [x]);
		
		/* to have the packet bytes aligned in columns, as a byte more than 16 will need 
		 * more than 1 character on the console.
		 */
		if (packet [x] < 16)
			printf (" ");

		/* if we printed 10 bytes, continue on a new line, else, continue on the same line */
		if (y >= 10)
		{
			printf ("\n");
			y = 1;
		}
		else
			printf (" ");
		
		x++;
		y++;
	}
	printf ("\n\n---PACKET END---\n\n");
}
