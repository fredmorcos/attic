#include "help.h"

#include <iostream>
#include <string>

using namespace std;

void help_print_version (const string &progname) {
  const string name = "PET";
  const string desc = "Personal Expense Tracker";
  const string version = "0.2";
  const string www = "https://github.com/fredmorcos/pet.git";
  const string cc = "Copyright (c) 2013";
  const string license = "2-clause BSD license";
  const string author = "Fred Morcos";
  const string email = "fred.morcos@gmail.com";

  cerr << name << " (" << progname << ") :"
       << desc << " -- Version " << version << endl
       << "WWW: " << www << endl
       << cc << " under a " << license << endl
       << author << " <" << email << ">" << endl;
}

void help_print_help (const string &progname) {
  help_print_version(progname);
  cerr << endl
       << "Usage:" << endl
       << "  " << progname << " [global_opts] [command] [local_opts]" << endl
       << endl
       << "Global Options:" << endl
       << "  --verbose,-v     Verbose output" << endl
       << "  --debug,-d       Debug output, implies verbose output" << endl
       << "  --file,-f FILE   Expenses file to read and/or manipulate" << endl
       << endl
       << "Commands:" << endl
       << "  check            Checks expenses file for errors" << endl;
}
