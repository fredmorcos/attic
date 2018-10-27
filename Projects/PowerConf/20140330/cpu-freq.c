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

#include "cpu-freq.h"
#include "map.h"
#include "file.h"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <glob.h>
#include <string.h>
#include <errno.h>

int glob_errfunc (const char *epath, int errno)
{
  fprintf (stderr, LOGSTR("GLOB error with %s: %s"),
	   __FILE__, __FUNCTION__, __LINE__, epath, strerror (errno));
  return 0;
}

int cpu_freq (int value)
{
  /* path to cpu frequency governors */
  const char *const path =
    "/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor";

  /* ondemand governor name */
  const char *const gov_ondemand = "ondemand";

  /* performance governor name */
  const char *const gov_performance = "performance";

  /* maximum value, 0,1=ondemand, 2=performance */
  const int max_value = 2;

  int mapped_value;		/* mapped local value */
  int file_write_ret;		/* file_write_*() return code */
  int glob_ret;			/* glob() return code */
  int i;			/* governor iterator */
  glob_t pglob;			/* contains list of matches */

  /* try to map the value from global to local */
  mapped_value = map_value (value, 0, max_value);

  if (mapped_value < 0)		/* if mapping fails */
    {
      /* log the error */
      fprintf (stderr, LOGSTR("Cannot map value %d -> %d"),
	       __FILE__, __FUNCTION__, __LINE__, value, mapped_value);

      return EXIT_FAILURE;	/* return with failure */
    }

  glob_ret = glob (path, GLOB_NOSORT,
		   (int (*) (const char *, int)) glob_errfunc,
		   &pglob);

  if (glob_ret != 0)
    {
      if (glob_ret == GLOB_NOSPACE)
	{
	  fprintf (stderr, LOGSTR ("GLOB out of memory error"),
		   __FILE__, __FUNCTION__, __LINE__);
	}
      else if (glob_ret == GLOB_ABORTED)
	{
	  fprintf (stderr, LOGSTR ("GLOB read error"),
		   __FILE__, __FUNCTION__, __LINE__);
	}
      else if (glob_ret == GLOB_NOMATCH)
	{
	  fprintf (stderr, LOGSTR ("GLOB no matches"),
		   __FILE__, __FUNCTION__, __LINE__);
	}
      else
	{
	  fprintf (stderr, LOGSTR ("GLOB should not be reached!"),
		   __FILE__, __FUNCTION__, __LINE__);
	}

      globfree (&pglob);
      return EXIT_FAILURE;
    }

  for (i = 0; i < pglob.gl_pathc; i++)
    {
      /* if ondemand */
      if (mapped_value == 0 || mapped_value == 1)
	{
	  /* status update */
	  fprintf (stdout, LOGSTR ("Writing %s to %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   gov_ondemand, pglob.gl_pathv[i]);

	  /* try to write file */
	  file_write_ret = file_write_str (pglob.gl_pathv[i],
					   gov_ondemand);
	}
      else if (mapped_value == 2)
	{
	  /* status update */
	  fprintf (stdout, LOGSTR ("Writing %s to %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   gov_performance, pglob.gl_pathv[i]);

	  /* try to write file */
	  file_write_ret = file_write_str (pglob.gl_pathv[i],
					   gov_performance);
	}
      else			/* this should not be reached */
	{
	  /* log the disaster */
	  fprintf (stderr, LOGSTR ("Should not happen!"),
		   __FILE__, __FUNCTION__, __LINE__);

	  return EXIT_FAILURE; /* return with failure */
	}

      if (file_write_ret == EXIT_FAILURE) /* if write failed */
	{
	  /* log the error */
	  fprintf (stderr, LOGSTR ("Error writing file %s"),
		   __FILE__, __FUNCTION__, __LINE__,
		   pglob.gl_pathv[i]);
	}
    }

  globfree (&pglob);
  return EXIT_SUCCESS;		/* return success */
}
