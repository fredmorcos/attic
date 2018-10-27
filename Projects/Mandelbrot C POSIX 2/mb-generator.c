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

#include "mb-generator.h"

#include <stddef.h>
#include <assert.h>

#ifdef OUTPUT_IMAGE
void generator_render(
		double min_r, double max_r, double min_i, double max_i, cairo_t *ct)
#else
void generator_render(
		double min_r, double max_r, double min_i, double max_i)
#endif
{
#ifdef OUTPUT_IMAGE
	assert(ct != NULL);
#endif

	double step_r = (max_r - min_r) / IMAGE_WIDTH,
				 step_i = (max_i - min_i) / IMAGE_HEIGHT,
				 c_r, c_i, z_r, z_new_r, z_i, z_new_i;
	int k = 0;

	for (int i = 0; i < IMAGE_WIDTH; i++)
	{
		for (int j = 0; j < IMAGE_HEIGHT; j++)
		{
			c_r = min_r + i * step_r;
			c_i = min_i + j * step_i;

			z_r = z_i = 0.;
			k = 0;

			while (k < MAX_ITER && ((z_r * z_r + z_i * z_i) < MAX_Z))
			{
				z_new_r = z_r * z_r - z_i * z_i + c_r;
				z_new_i = 2 * z_r * z_i + c_i;

				z_r = z_new_r;
				z_i = z_new_i;

				k++;
			}

#ifdef OUTPUT_IMAGE
			if (k == MAX_ITER)
				cairo_set_source_rgb(ct, 0, 0, 0);
			else
			{
				double k_new = (k % 256) / 256.;
				cairo_set_source_rgb(ct, k_new, k_new, k_new);
			}

			cairo_rectangle(ct, i, j, 1, 1);
			cairo_fill(ct);
#endif
		}
	}
}

