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

#include "command.h"
#include "extra.h"

#include <stdlib.h>

void exec_command (char *cmd, command *list,
                   int argc, char **argv,
                   global_opts *gopts, error *err)
{
  while (list->cmd != NULL) {
    if (strieq(cmd, list->cmd)) {
      list->func(argc, argv, gopts, err);
      return;
    }
    list++;
  }

  err = mkerr(1, ENOCMD, 1, cmd);
}

int is_command (char *cmd, command *list)
{
  while (list->cmd != NULL) {
    if (strieq(cmd, list->cmd))
      return 1;
    list++;
  }

  return 0;
}
