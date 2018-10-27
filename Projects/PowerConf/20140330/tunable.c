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

#include "tunable.h"
#include "map.h"
#include "file.h"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <glob.h>


int glob_errfunc (const char *epath, int errno)
{
  fprintf (stderr, LOGSTR("GLOB error with %s: %s"),
	   __FILE__, __FUNCTION__, __LINE__,
	   epath, strerror (errno));
  return 0;
}

int write_tunable (tunable_t *t, const int value)
{
  int mapped_val;
  int retval;
  int i;
  glob_t pglob;

  if (t->is_str_range)
    {
      t->minval = 0;
      t->maxval = sizeof (t->str_values);
    }

  mapped_val = map_value (value, t->minval, t->maxval);

  if (mapped_val < 0)		/* if mapping failed */
    {
      fprintf (stderr, LOGSTR ("Cannot map value %d -> %d"),
	       __FILE__, __FUNCTION__, __LINE__,
	       value, mapped_val);

      return EXIT_FAILURE;	/* return failure */
    }

  if (t->invert)
    mapped_val = t->maxval - mapped_val;

  fprintf (stdout, LOGSTR ("GLOB matching %s"),
	   __FILE__, __FUNCTION__, __LINE__,
	   t->path);

  retval = glob (t->path, GLOB_NOSORT,
		 (int (*) (const char *, int)) glob_errfunc,
		 &pglob);

  if (glob_ret != 0)
    {
      if (glob_ret == GLOB_NOSPACE)
	fprintf (stderr, LOGSTR ("GLOB out of memory error"),
		 __FILE__, __FUNCTION__, __LINE__);
      else if (glob_ret == GLOB_ABORTED)
	fprintf (stderr, LOGSTR ("GLOB read error"),
		 __FILE__, __FUNCTION__, __LINE__);
      else if (glob_ret == GLOB_NOMATCH)
	fprintf (stderr, LOGSTR ("GLOB no matches"),
		 __FILE__, __FUNCTION__, __LINE__);
      else
	fprintf (stderr, LOGSTR ("GLOB should not be reached!"),
		 __FILE__, __FUNCTION__, __LINE__);

      globfree (&pglob);
      return EXIT_FAILURE;
    }

  for (i = 0; i < pglob.gl_pathc; i++)
    {
      if (t->is_str_range)
	{
	  fprintf (stdout, LOGSTR ("Writing %s to %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   t->str_values[mapped_value], pglob.gl_pathv[i]);
	  retval = file_write_str (pglob.gl_pathv[i],
				   t->str_values[mapped_value]);
	}
      else
	{
	  fprintf (stdout, LOGSTR ("Writing %d to %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   mapped_value, pglob.gl_pathv[i]);
	  retval = file_write_int (pglob.gl_pathv[i],
				   mapped_value);
	}

      if (retval == EXIT_FAILURE) /* if write failed */
	fprintf (stderr, LOGSTR ("Error writing file %s"),
		 __FILE__, __FUNCTION__, __LINE__,
		 pglob.gl_pathv[i]);
    }

  globfree (&pglob);
  return EXIT_SUCCESS;
}
