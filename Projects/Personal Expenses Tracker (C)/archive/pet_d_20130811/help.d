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

import std.stdio;

import info;

void show_help (bool show_header=true, bool show_body=true) {
  if (show_header) {
    stderr.writeln("PET - Personal Expense Tracker - Version ", petVersion);
    stderr.writeln("Copyright (C) 2013 - Fred Morcos <fred.morcos@gmail.com>");
    stderr.writeln("Licensed under the GNU GPLv3 - See LICENSE file");
    stderr.writeln("http://github.com/fredmorcos/pet.git");
  }

  if (show_body) {
    stderr.writeln();
    stderr.writeln("usage");
    stderr.writeln("  pet [arguments...] [--command=...|-c ...] FILE");
    stderr.writeln();
    stderr.writeln("arguments");
    stderr.writeln("  --help      -h   Show this help");
    stderr.writeln("  --version   -x   Show program version");
    stderr.writeln("  --verbose   -v   Enable verbose output");
    stderr.writeln("  --extended  -e   Show extended dates");
    stderr.writeln("  --output    -o   Output file");
    stderr.writeln();
    stderr.writeln("commands");
    stderr.writeln("  show             Show all expenses with stats");
    stderr.writeln("  add              Add an expense item");
    stderr.writeln("  iadd             Interactively add an expense item");
  }
}
