#!/usr/bin/env python

import sys

class UnknownArgException(Exception):
  def __init__(self, argname):
    self.argname = argname

  def __str__(self):
    return "Unknown argument: %s" % self.argname

class FileAlreadyGivenException(Exception):
  def __init__(self, oldfilename, newfilename):
    self.oldfilename = oldfilename
    self.newfilename = newfilename

  def __str__(self):
    return ("Extra filename given: %s\n"
            " -> Already given: %s" %
            (self.newfilename, self.oldfilename))

class CommandLineOptions:
  def __init__(self):
    self.verbose = False
    self.filename = None
    self.version = False
    self.help = False

  def __str__(self):
    return ("Command Line Options\n"
            " -> verbose = %s\n"
            " -> filename = %s\n"
            " -> version = %s\n"
            " -> help = %s" %
            (self.verbose, self.filename, self.version, self.help))

def parse_cmdline_opts(args, acc=CommandLineOptions()):
  if len(args) == 0:
    return acc
  else:
    if args[0] in ["-v", "--verbose"]:
      acc.verbose = True
    elif args[0] in ["-V", "--version"]:
      acc.version = True
    elif args[0] in ["-h", "--help"]:
      acc.help = True
    else:
      if args[0].startswith("-"):
        raise UnknownArgException(args[0])
      else:
        if acc.filename == None:
          acc.filename = args[0]
        else:
          raise FileAlreadyGivenException(acc.filename, args[0])
    return parse_cmdline_opts(args[1:], acc)

def main():
  args = sys.argv[1:]

  if len(args) == 0:
    print(("Error: "
           "Not enough arguments\n"
           " -> See `%s --help' for more information" % sys.argv[0]))
    exit(1)

  try:
    opts = parse_cmdline_opts(args)
  except UnknownArgException as e:
    print("Error:", e)
    exit(1)
  except FileAlreadyGivenException as e:
    print("Error:", e)
    exit(1)

if __name__ == "__main__":
  main()
