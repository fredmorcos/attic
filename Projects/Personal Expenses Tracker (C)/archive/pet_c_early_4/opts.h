#ifndef __PET_OPTS__
#define __PET_OPTS__

struct gopts {
  int verbose;
  int debug;
  char *file;
};

int get_gopts (int, char**, struct err*);

#endif
