#ifndef __PET_OPTS__
#define __PET_OPTS__

#include <string>
#include <vector>

using namespace std;

struct OptsGlobal {
  bool verbose = false;
  bool debug = false;
  string file;
};

int opts_global (const string&, const vector<string>&, OptsGlobal&);

#endif
