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

#ifndef __SESSION
#define __SESSION

#include <pcap.h>

#include "settings.h"

typedef struct _Packet {
	struct	pcap_pkthdr		*header;
	const	u_char			*data;
} Packet;

typedef struct _Session {
	pcap_t		*handle;
	char		*interface;
	Settings	*settings;
	
	void		(*onPacketArrive) (u_char *args, const struct pcap_pkthdr *header, const u_char *packet);
} Session;

inline Session		*newSession		(void);
void				openSession		(Session *s);
void				startSession	(Session *s);
inline void			closeSession	(Session *s);

#endif
