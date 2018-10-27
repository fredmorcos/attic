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

import std.datetime;
import std.conv;
import std.array;
import std.string;
import std.algorithm;

import document;

string[] parsePersons(string line) {
  return map!string(a => trim(a))(split(line, ","));
}

Document parseExpenses (string data) {
  string[] lines = splitLines(data);
  string[] persons = parsePersons(lines[0]);

  return null;
}
