#include "cmd.h"
#include "help.h"

#include <iostream>
#include <string>
#include <vector>

using namespace std;

int cmd_exec (const string &progname, const vector<string> &args) {
  if (args[0] == string("version"))
    help_print_version(progname);
  else if (args[0] == string("help"))
    help_print_help(progname);
  else {
    cerr << "Error: unknown command `" << args[0] << "`" << endl
	 << " -> See `" << progname << " help`" << endl;
    return 1;
  }

  return 0;
}
