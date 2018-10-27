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

#ifndef __SETTINGS
#define __SETTINGS

#include "global.h"

typedef struct _Settings {
	bool	std;
	int		wid;
	bool	file;
	char	*filename;
} Settings;

inline Settings		*newSettings	(void);
inline void			closeSettings	(Settings *s);

#endif
