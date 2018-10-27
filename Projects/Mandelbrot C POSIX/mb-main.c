/*
 *	This file is part of Fred's Mandelbrot.
 *
 *	Fred's Mandelbrot is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Fred's Mandelbrot is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Fred's Mandelbrot.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mb-generator.h"

int main (int argc, char *argv[])
{
	double min_r, max_r, min_i, max_i;

	if (argc < 4)
	{
help:
		puts("Usage: ./mand <threads> <cols-per-thread> <--locks|--no-locks>");
		exit(1);
	}

	min_r = -2;
	max_r = 1;
	min_i = -1;
	max_i = 1;

	if (strcmp("--locks", argv[3]) == 0)
	{
		generator_render(min_r, max_r, min_i, max_i, atoi(argv[1]), atoi(argv[2]));
	}
	else if (strcmp("--no-locks", argv[3]) == 0)
	{
		generator_render_no_locks(min_r, max_r, min_i, max_i, atoi(argv[1]), atoi(argv[2]));
	}
	else
	{
		goto help;
	}

	return 0;
}

