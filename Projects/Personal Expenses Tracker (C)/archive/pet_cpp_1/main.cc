#include <iostream>
#include <string>
#include <vector>

#include "cmd.h"
#include "opts.h"

using namespace std;

int main (int argc, char **argv) {
  vector<string> args;
  OptsGlobal gopts;
  int ret;

  const string progname = argv[0];

  if (argc < 2) {
    cerr << "Error: too little arguments" << endl
	 << " -> See `" << progname << " help`" << endl;
    return 1;
  }

  for (int i = 1; i < argc; ++i)
    args.push_back(argv[i]);

  ret = opts_global (progname, args, gopts);
  switch (ret) { case 0: break; case 1: return 1; }

  ret = cmd_exec (progname, args);
  switch (ret) { case 0: break; case 1: return 1; }

  return 0;
}
