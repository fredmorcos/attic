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

#ifndef __TUNABLE_H__
#define __TUNABLE_H__

typedef struct _tunable_t {
  char *name;		/* tunable name */
  char *path;		/* path or glob to sys iface files */
  int minval;		/* min param value */
  int maxval;		/* max param value */
  int invert;		/* whether to invert the mapping */
  int is_str_range;	/* values are strings */
  char **str_values;	/* set of string params/values */
} tunable_t;

int write_tunable (tunable_t *t, const int value);

#endif	/* __TUNABLE_H__ */
