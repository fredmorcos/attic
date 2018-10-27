/* This file is part of powerconf.
 *
 * powerconf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * powerconf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with powerconf.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "map.h"
#include "config.h"
#include <stdio.h>

int map_value (int value, int min_level, int max_level)
{
  int total_global_levels;	/* total num of global levels */
  int total_levels;		/* total num of local levels */
  double factor;		/* factor map local <> global */
  double mapped_value;		/* final result, mapped value */

  /* map from the global level to the given local level */
  total_global_levels = MAX_GLOBAL_LEVEL - MIN_GLOBAL_LEVEL + 1;
  total_levels = max_level - min_level + 1;
  factor = ((double) total_global_levels) / ((double) total_levels);
  mapped_value = ((double) value) / (double) factor;

  /* return mapped value */
  return ((int) mapped_value) + min_level;
}
