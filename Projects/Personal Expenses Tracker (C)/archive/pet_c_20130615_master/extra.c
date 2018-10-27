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

#include "extra.h"

#include <stdio.h>
#include <ctype.h>

#define EOS '\0'

void putserr (const char *msg)
{
  fputs(msg, stderr);
  fputs("\n", stderr);
  fflush(stderr);
}

int strieq (char *a, char *b)
{
  while (*a != EOS && *b != EOS) {
    if (tolower(*a) != tolower(*b))
      return 0;
    a++;
    b++;
  }

  if (*a != EOS || *b != EOS)
    return 0;
  return 1;
}
