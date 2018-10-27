/*
 * This file is part of PET.
 *
 * PET is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PET is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with PET.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __PET_OPTS__
#define __PET_OPTS__

typedef struct _global_opts {
  int verbose;                  /* verbose output */
} global_opts;

typedef struct _show_opts {
  int ext_dates;                /* show dates in textual format */
} show_opts;

#endif  /* __PET_OPTS__ */
