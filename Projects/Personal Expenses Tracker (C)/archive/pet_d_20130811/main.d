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
import std.getopt;
import std.file;
import std.conv;

import help;
import logging;
import document;
import parser;
import expense;

enum Command { show, add, iadd };

int main (string[] args) {
  bool verbose;                 // verbose output
  bool debug_;                  // debug output
  bool extended;                // extended dates
  bool help;                    // show help
  bool ver;                     // show version
  Command cmd;                  // command
  string output;                // output file
  string[] subargs;             // remainder of arguments

  Document document;
  Logger log = new Logger();

  if (args.length <= 1) {
    log.writeError("No arguments given.");
    show_help(false);
    return 1;
  }

  try {
    getopt(args, std.getopt.config.bundling,
           "verbose|v",  &verbose,
           "debug|d",    &debug_,
           "version|x",  &ver,
           "extended|e", &extended,
           "help|h",     &help,
           "command|c",  &cmd,
           "output|o",   &output);
  } catch (Exception e) {
    log.writeError(e.msg);
    show_help(false);
    return 1;
  }

  if (help) {
    show_help();
    return 0;
  }

  if (ver) {
    show_help(true, false);
    return 0;
  }

  // logger settings
  log.enableVerbose(verbose);
  log.enableDebug(debug_);

  subargs = args[1 .. args.length]; // dangerous assumption?

  if (subargs.length < 1) {
    log.writeError("No file given.");
    show_help(false);
    return 1;
  } else if (subargs.length > 1) {
    log.writeError("Too many files given: ", subargs);
    show_help(false);
    return 1;
  }

  // safely assume that subargs.length == 1
  log.writeInfo("Input file: ", subargs[0]);

  if (output.length == 0) {
    output = subargs[0];
    log.writeInfo("Output file not given. Using ", output);
  }

  log.writeInfo("Output: ", output);
  log.writeInfo("Command: ", cmd);

  try {
    document = parseExpenses(to!string(read(subargs[0])));
  } catch (Exception  e) {
    log.writeError("Error: ", e.msg);
    return 1;
  }

  return 0;
}
