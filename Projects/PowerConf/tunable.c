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

int glob_errfunc (const char *epath, int errno);
int tunable_str_values_length (tunable_t *t);

int glob_errfunc (const char *epath, int errno)
{
  fprintf (stderr, LOGSTR ("GLOB error with %s: %s"),
	   __FILE__, __FUNCTION__, __LINE__,
	   epath, strerror (errno));
  return 0;
}

int tunable_str_values_length (tunable_t *t)
{
  char **p;
  int i;

  p = t->str_values;

  while (*p)
    {
      i++;
      p++;
    }

  return i;
}

int write_tunable (tunable_t *t, const int value)
{
  int mval;			/* mapped local value */
  int retval;			/* return code */
  int i;			/* iterator */
  glob_t pglob;

  /* map the value from the global power level to a local value */
  if (t->is_str_range)
    {
      t->minval = 0;
      t->maxval = tunable_str_values_length (t) - 1;
    }
  mval = map_value (value, t->minval, t->maxval);

  if (mval < 0)		/* if mapping failed */
    {
      fprintf (stderr, LOGSTR ("Cannot map value %d -> %d"),
	       __FILE__, __FUNCTION__, __LINE__,
	       value, mval);
      return EXIT_FAILURE;
    }

  if (t->invert)		/* if inversion is needed */
    mval = t->maxval - mval;

  retval = glob (t->path, GLOB_NOSORT,
		 (int (*) (const char *, int)) glob_errfunc,
		 &pglob);

  if (retval != 0)
    {
      if (retval == GLOB_NOSPACE)
	fprintf (stderr, LOGSTR ("GLOB out of memory error"),
		 __FILE__, __FUNCTION__, __LINE__);
      else if (retval == GLOB_ABORTED)
	fprintf (stderr, LOGSTR ("GLOB read error"),
		 __FILE__, __FUNCTION__, __LINE__);
      else if (retval == GLOB_NOMATCH)
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
		   t->str_values[mval], pglob.gl_pathv[i]);

	  /* try to write file */
	  retval = file_write_str (pglob.gl_pathv[i],
				   t->str_values[mval]);

	  if (retval == EXIT_FAILURE)
	    fprintf (stderr, LOGSTR ("Error writing %s to %s"),
		     __FILE__, __FUNCTION__, __LINE__,
		     t->str_values[mval], pglob.gl_pathv[i]);
	}
      else
	{
	  fprintf (stdout, LOGSTR ("Writing %d to %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   mval, pglob.gl_pathv[i]);

	  /* try to write the file */
	  retval = file_write_int (pglob.gl_pathv[i], mval);

	  /* file write failed */
	  if (retval == EXIT_FAILURE)
	    fprintf (stderr, LOGSTR ("Error writing %d to %s"),
		     __FILE__, __FUNCTION__, __LINE__,
		     mval, pglob.gl_pathv[i]);
	}
    }

  globfree (&pglob);		/* free globbing resources */
  return EXIT_SUCCESS;		/* all good, return success */
}
