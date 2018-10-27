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

#ifndef __PET_COMMAND__
#define __PET_COMMAND__

#include "opts.h"
#include "error.h"

typedef struct _command {
  char *cmd;
  int (*func) (int, char **, global_opts *, error *);
} command;

/* command helpers */
int is_command (char *, command *);
void exec_command (char *, command *, int, char **, global_opts *, error *);

#endif  /* __PET_COMMAND__ */
