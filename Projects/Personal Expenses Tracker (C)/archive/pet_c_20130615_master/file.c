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

#include "file.h"

#include <stdio.h>
#include <fcntl.h>
#include <err.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/stat.h>

char *readfile (const char *filename, int *size)
{
  char *data = NULL;
  FILE *f = NULL;
  int fd;
  int read;
  struct stat sb;

  fd = open(filename, O_RDONLY);

  if (fd == -1) {
    warn("%s: cannot open file %s", __FUNCTION__, filename);
    return NULL;
  }

  if (fstat(fd, &sb) == -1) {
    warn("%s: cannot stat file %s", __FUNCTION__, filename);
    goto finish_err;
  }

  if (!S_ISREG(sb.st_mode)) {
    warn("%s: %s is not a file", __FUNCTION__, filename);
    goto finish_err;
  }

  if (close(fd) == -1) {
    warn("%s: cannot close fd for %s", __FUNCTION__, filename);
    return NULL;
  }

  f = fopen(filename, "r");

  if (!f) {
    warn("%s: cannot open file %s", __FUNCTION__, filename);
    goto finish_err;
  }

  data = (char *) malloc(sizeof(char) * sb.st_size);

  if (!data) {
    warn("%s: cannot allocate space for file %s", __FUNCTION__, filename);
    goto finish_err;
  }

  read = fread(data, 1, sb.st_size, f);

  if (read < sb.st_size) {
    warn("%s: data read is less than requested for %s", __FUNCTION__, filename);
    goto finish_err;
  }

  if (fclose(f) != 0) {
    warn("%s: cannot close file %s", __FUNCTION__, filename);
    goto finish_err_late;
  }

  *size = sb.st_size;
  return data;

 finish_err:
  if (fclose(f) != 0)
    warn("%s: cannot close file %s", __FUNCTION__, filename);

 finish_err_late:
  if (data)
    free(data);

  return NULL;
}
