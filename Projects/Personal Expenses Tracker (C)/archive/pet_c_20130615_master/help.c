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

#include "help.h"
#include "extra.h"

#include <stdio.h>

#define VERSION "0.2"

void show_version ()
{
  putserr("PET -- The Personal Expense Tracker -- Version " VERSION);
  putserr("Copyright 2012-2013 Fred Morcos <fred.morcos@gmail.com>");
  putserr("http://github.com/fredmorcos/pet.git");
  putserr("Licensed under the GPLv3");
}

void show_help ()
{
  putserr("usage");
  putserr("  pet [global-options] <command> [command-options] [FILE ...]");
  putserr("");
  putserr("commands");
  putserr("  add             Add an expense");
  putserr("  show            Show expenses");
  putserr("  help            Show this help");
  putserr("  optimize        Optimize expenses file");
  putserr("  version         Show version info");
  putserr("");
  putserr("global options");
  putserr("  --verbose,-v    Be verbose with output");
  putserr("");
  putserr("options for show");
  putserr("  --extended/-e   Print extended dates");
}
