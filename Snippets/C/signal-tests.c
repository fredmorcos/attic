#include <stdio.h>
#include <bsd/bsd.h>
#include <unistd.h>
#include <signal.h>
#include <stdbool.h>

typedef struct sigaction sa_t;

static bool stop = false;

void sig_h(int s) {
  warnx("got signal %d", s);
  stop = true;
}

int main (void) {
  sigaction(SIGTERM, &(sa_t){ .sa_handler = sig_h }, NULL);
  sigaction(SIGINT,  &(sa_t){ .sa_handler = sig_h }, NULL);
  sigaction(SIGABRT, &(sa_t){ .sa_handler = sig_h }, NULL);

  while (!stop)
    sleep(1);

  stop = false;
  warnx("Decided not to finish...");

  while (!stop)
    sleep(1);

  return 0;
}
