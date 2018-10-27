#!/usr/bin/env python

import argparse
import sys
import fileinput
import parser

VERSION = "0.2"

if __name__ == "__main__":
  ap = argparse.ArgumentParser(description="Personal Expense Tracker")
  ap.add_argument("FILE", help="File to manipulate")
  ap.add_argument("COMMAND",
                  help="Command to execute",
                  choices=["show", "add"])
  ap.add_argument("-V", "--version",
                  help="Show version information",
                  action="store_true")
  ap.add_argument("-v", "--verbose",
                  help="Verbose output",
                  action="store_true")
  args = ap.parse_args()

  if args.version:
    print("PET -- The Personal Expense Tracker -- Version %s" % VERSION)
    print("Copyright 2012-2013 Fred Morcos <fred.morcos@gmail.com>")
    print("http://github.com/fredmorcos/pet.git")
    print("Licensed under the GPLv3")
    sys.exit(0)

  if args.verbose:
    print("File: %s" % args.FILE)

  try:
    with fileinput.input(files=(args.FILE)) as f:
      for l in f:
        print(parser.get_expense(l))
  except OSError as e:
    print(e)
    sys.exit(e.errno)
