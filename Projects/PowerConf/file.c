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

#include "file.h"
#include "debug.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int file_write_int (const char *path, const int data)
{
  char value_str[255];		/* string rep of int */
  int sprintf_ret;		/* sprintf() return code */

  /* try to convert int to string */
  sprintf_ret = sprintf (value_str, "%d", data);

  if (sprintf_ret < 0)		/* conversion failed */
    {
      /* log error */
      fprintf (stderr, LOGSTR("Cannot convert int %d"),
	       __FILE__, __FUNCTION__, __LINE__, data);

      return EXIT_FAILURE;	/* return with failure */
    }

  return file_write_str (path, (const char *) value_str);
}

int file_write_str (const char *path, const char *data)
{
  FILE *file;			/* file descriptor */
  int errn;			/* error code */
  int fclose_rc;		/* return code from fclose() */
  int ret_code;			/* function return code */
  size_t fwrite_len;		/* length of written data */

  ret_code = EXIT_SUCCESS;	/* assume we will succeed */

  file = fopen (path, "w");	/* open file for writing */

  if (!file)			/* if opening file failed */
    {
      errn = errno;		/* save error code and log */
      fprintf (stderr, LOGSTR("Cannot open %s: %s"),
	       __FILE__, __FUNCTION__, __LINE__, path, strerror (errn));

      return EXIT_FAILURE;	/* return with failure */
    }

  /* try to write the data to file */
  fwrite_len = fwrite (data, strlen (data), 1, file);

  if (fwrite_len < 1)		/* write failed */
    {
      errn = errno;		/* save error code and log */
      fprintf (stderr, LOGSTR("Cannot write %s: %s"),
	       __FILE__, __FUNCTION__, __LINE__, path, strerror (errn));

      /* here we only save the return code (as failure) because we
	 need to close the file descriptor first before returning */
      ret_code = EXIT_FAILURE;
    }

  fclose_rc = fclose (file);	/* try to close the file */

  if (fclose_rc == EOF)		/* closing failed */
    {
      errn = errno;		/* save error code and log */
      fprintf (stderr, LOGSTR("Cannot close %s: %s"),
	       __FILE__, __FUNCTION__, __LINE__, path, strerror (errn));

      return EXIT_FAILURE;	/* return with failure */
    }

  if (ret_code == EXIT_SUCCESS)	/* log a success message */
    fprintf (stdout, LOGSTR("Wrote %s to %s"),
	     __FILE__, __FUNCTION__, __LINE__, data, path);

  /* here we will return with either success or failure from the
     writing phase */
  return ret_code;
}
