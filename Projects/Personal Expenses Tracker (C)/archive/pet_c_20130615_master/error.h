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

#ifndef __PET_ERROR__
#define __PET_ERROR__

#define EINARG 1
#define ENOARG 2
#define EINCMD 3
#define ENOCMD 4

#define mkerr(u, c, h, p) _mkerr(u, c, h, __LINE__, (char *) __FUNCTION__, p)

typedef struct _error {
  int user;                     /* user-defined or errno */
  int code;                     /* error-code */
  int help;                     /* show help message */
  int line;                     /* line of code where error occurred */
  char *func;                   /* function name where error occurred */
  char *param;                  /* data parameter */
} error;

error *_mkerr(int, int, int, int, char *, char *);
char *etos(error *);

#endif  /* __PET_ERROR__ */
