#include "opts.h"

#include <iostream>
#include <vector>
#include <string>

using namespace std;

int opts_global (const string& progname, const vector<string>& args,
		 OptsGlobal& gopts) {
  string arg;

  for (unsigned long i = 0; i < args.size(); ++i) {
    arg = args[i];

    if (arg == string("-v") || arg == string("--verbose"))
      gopts.verbose = true;
    else if (arg == string("-d") || arg == string("--debug")) {
      gopts.verbose = true;
      gopts.debug = true;
    } else if (arg == string("-f") || arg == string("--file")) {
      if (i >= args.size() - 1) {
	cerr << "Error: no file given for `-f` argument" << endl
	     << " -> See `" << progname << " help`" << endl;
	return 1;
      } else {
	gopts.file = args[i + 1];
	++i;
      }
    } else { break; }
  }

  return 0;
}
