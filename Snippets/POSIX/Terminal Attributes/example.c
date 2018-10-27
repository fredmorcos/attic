#include "term.h"

void loading(const char *const filename);

void loading(const char *const filename) {
  static const enum termattr attr[] = {
    TermAttrFGLightYellow,
    TermAttrBold,
        TermAttrEnd
  };

  static const enum termattr attr_end[] = {
    TermAttrReset,
        TermAttrEnd
  };

  termset(stdout, attr_end);
  fclearline(stdout);
  termset(stdout, attr);
  printf("Loading:");
  termset(stdout, attr_end);
  printf(" %s", filename);
  fflush(stdout);

  usleep(500);
}
