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
#include <stdlib.h>

#define IMAGE_WIDTH		3200
#define IMAGE_HEIGHT	2400

static pthread_mutex_t current_column_mutex = PTHREAD_MUTEX_INITIALIZER;
static int current_column;
static int columns_per_thread;

static double step_r, step_i, min_r, max_r, min_i, max_i;

void *generator_render_part(void *);
void *generator_render_part_no_locks(void *);
int generator_get_next_column();

void generator_render_no_locks(
		double mini_r, double maxi_r, double mini_i, double maxi_i,
		int threads_n, int cols_n)
{
	assert(cols_n > 0);
	assert(threads_n > 0);

	columns_per_thread = IMAGE_WIDTH / threads_n;

	min_r = mini_r;
	max_r = maxi_r;
	min_i = mini_i;
	max_i = maxi_i;

	step_r = (max_r - min_r) / IMAGE_WIDTH;
	step_i = (max_i - min_i) / IMAGE_HEIGHT;

	pthread_t threads[threads_n];

	int *thread_i;

	for (int i = 0; i < threads_n; i++)
	{
		thread_i = (int *) malloc(sizeof(int));
		*thread_i = i;
		pthread_create(&threads[i], NULL, generator_render_part_no_locks, (void *) thread_i);
	}

	for (int i = 0; i < threads_n; i++)
		pthread_join(threads[i], NULL);
}

void *generator_render_part_no_locks(void *param)
{
	int thread_i = *((int *)(param));

	double c_r, c_i, z_r, z_new_r, z_i, z_new_i;
	int k = 0, prv_current_column;

	prv_current_column = thread_i * columns_per_thread;

	for (int x = columns_per_thread; x >= 0; x--)
	{
		if ((prv_current_column + x) >= IMAGE_WIDTH)
			pthread_exit(0);

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

		}
	}

	pthread_exit(0);
}

void generator_render(
		double mini_r, double maxi_r, double mini_i, double maxi_i,
		int threads_n, int cols_n)
{
	assert(cols_n > 0);
	assert(threads_n > 0);

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

