#include <errno.h>
#include <stdio.h>
#include <err.h>
#include <unistd.h>
#include <signal.h>
#include <stdbool.h>
#include <sysexits.h>
#include <stdlib.h>

typedef struct sigaction sa_t;

void sig_h(int s) {
  warnx("got signal %d", s);
}

int main (void) {
  sigaction(SIGTERM, &(sa_t){ .sa_handler = sig_h }, NULL);
  sigaction(SIGABRT, &(sa_t){ .sa_handler = sig_h }, NULL);

#define READSIZE 1000 * 1024 * 1024

  char *p = malloc(READSIZE);
  if (!p)
    err(EX_OSERR, "Cannot allocate %d bytes", READSIZE);

  FILE *f = fopen("/dev/urandom", "r");
  if (!f) {
    free(p);
    err(EX_OSERR, "Cannot open /dev/urandom for reading");
  }
  warnx("Reading from /dev/urandom...");
  int fd = fileno(f);
  ssize_t res = read(fd, p, READSIZE);
  if (res < READSIZE) {
    if (errno == EINTR)
      err(EX_OK, "Interrupted by signal");
    warn("Got a signal");
  }
  warnx("Bye bye");
  fclose(f);
  free(p);

  return 0;
}
