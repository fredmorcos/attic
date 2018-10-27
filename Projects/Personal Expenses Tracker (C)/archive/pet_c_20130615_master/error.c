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

#include "error.h"

#include <stdlib.h>

error *_mkerr(int user, int code, int help, int line,
              char *func, char *param)
{
  error *err;

  err = (error *) malloc(sizeof(error));
  err->user = user;
  err->code = code;
  err->help = help;
  err->line = line;
  err->func = func;
  err->param = param;

  return err;
}

char *etos(error *err)
{
  char *msg;
  char *res;

  if (err->user) {
    switch (err->code) {
    case EINARG:
      msg = "invalid or unknown argument: ";
      res = (char *) malloc(strlen(err->))
        break;
    case ENOARG:
      msg = "no argument given";
      break;
    case EINCMD:
      msg = ""
        }
  } else {
  }
}
