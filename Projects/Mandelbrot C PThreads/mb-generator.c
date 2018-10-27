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

#include <pthread.h>
#include <assert.h>
#include <stddef.h>

#ifdef OUTPUT_IMAGE
static cairo_t *ct;
static pthread_mutex_t graphics_context_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

static pthread_mutex_t current_column_mutex = PTHREAD_MUTEX_INITIALIZER;
static int current_column;
static int columns_per_thread;

static double step_r, step_i, min_r, max_r, min_i, max_i;

void *generator_render_part(void *);
int generator_get_next_column();

#ifdef OUTPUT_IMAGE
void generator_render(
		double mini_r, double maxi_r, double mini_i, double maxi_i, cairo_t *context,
		int threads_n, int cols_n)
#else
void generator_render(
		double mini_r, double maxi_r, double mini_i, double maxi_i,
		int threads_n, int cols_n)
#endif
{
#ifdef OUTPUT_IMAGE
	assert(context != NULL);
#endif
	assert(cols_n > 0);
	assert(threads_n > 0);

#ifdef OUTPUT_IMAGE
	ct = context;
#endif

	columns_per_thread = cols_n;
	current_column = -columns_per_thread;

	min_r = mini_r;
	max_r = maxi_r;
	min_i = mini_i;
	max_i = maxi_i;

	step_r = (max_r - min_r) / IMAGE_WIDTH;
	step_i = (max_i - min_i) / IMAGE_HEIGHT;

	pthread_t threads[threads_n];

	for (int i = 0; i < threads_n; i++)
		pthread_create(&threads[i], NULL, generator_render_part, NULL);

	for (int i = 0; i < threads_n; i++)
		pthread_join(threads[i], NULL);

#ifdef OUTPUT_IMAGE
	pthread_mutex_destroy(&graphics_context_mutex);
#endif
	pthread_mutex_destroy(&current_column_mutex);
}

void *generator_render_part(void *param)
{
	double c_r, c_i, z_r, z_new_r, z_i, z_new_i;
	int k = 0, prv_current_column;

	while ((prv_current_column = generator_get_next_column()) != -1)
	{
		pthread_mutex_unlock(&current_column_mutex);

		for (int x = columns_per_thread; x >= 0; x--)
		{
			if ((prv_current_column + x) >= IMAGE_WIDTH)
				continue;

			for (int j = 0; j < IMAGE_HEIGHT; j++)
			{
				c_r = min_r + (prv_current_column + x) * step_r;
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
				pthread_mutex_lock(&graphics_context_mutex);

				if (k == MAX_ITER)
					cairo_set_source_rgb(ct, 0, 0, 0);
				else
				{
					double k_new = (k % 256) / 256.;
					cairo_set_source_rgb(ct, k_new, k_new, k_new);
				}
	
				cairo_rectangle(ct, prv_current_column + x, j, 1, 1);
				cairo_fill(ct);

				pthread_mutex_unlock(&graphics_context_mutex);
#endif
			}
		}
	}

	pthread_mutex_unlock(&current_column_mutex);
	pthread_exit(0);
}

int generator_get_next_column()
{
	pthread_mutex_lock(&current_column_mutex);

	if (current_column >= IMAGE_WIDTH)
		return -1;

	current_column += columns_per_thread;
	return current_column;
}

