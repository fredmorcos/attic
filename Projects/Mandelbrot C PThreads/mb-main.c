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

#ifdef OUTPUT_IMAGE
#include <cairo.h>
#endif

#include "mb-generator.h"

int main (int argc, char *argv[])
{
	double min_r, max_r, min_i, max_i;

	if (argc < 3)
	{
		puts("Usage: ./mandelbrot-parallel number-of-threads number-of-columns-per-thread");
		exit(1);
	}

#ifdef BENCHMARK
	min_r = -1.4;
	max_r = -1.32;
	min_i = -0.1;
	max_i = -0.02;
#else
	min_r = -2;
	max_r = 1;
	min_i = -1;
	max_i = 1;
#endif

#ifdef OUTPUT_IMAGE
	cairo_surface_t *surface;
	cairo_t *ct;

	surface = cairo_image_surface_create(
			CAIRO_FORMAT_RGB24, IMAGE_WIDTH, IMAGE_HEIGHT);
	ct = cairo_create(surface);

	generator_render(min_r, max_r, min_i, max_i, ct, atoi(argv[1]), atoi(argv[2]));

	cairo_surface_write_to_png(surface, "mandelbrot.png");

	cairo_destroy(ct);
	cairo_surface_destroy(surface);
#else
	generator_render(min_r, max_r, min_i, max_i, atoi(argv[1]), atoi(argv[2]));
#endif

	return 0;
}

